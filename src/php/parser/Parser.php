<?php


namespace parser;


use parser\action\Action;
use parser\action\ActionType;
use parser\action\ApplyAction;
use parser\action\EvalAction;
use parser\ast\AST;
use parser\ast\ASTs;
use parser\ast\Char;
use parser\ast\EmptyAST;
use parser\config\Automata;
use parser\config\ParserConfig;
use parser\continuation\AltContinuation;
use parser\continuation\AndContinuation;
use parser\continuation\Continuation;
use parser\continuation\Continuations;
use parser\continuation\ContinuationType;
use parser\continuation\NonTerminalContinuation;
use parser\continuation\NotContinuation;
use parser\continuation\OptContinuation;
use parser\continuation\PlusContinuation;
use parser\continuation\SeqContinuation;
use parser\continuation\StarContinuation;
use parser\continuation\VoidContinuation;
use parser\error\CharacterClassError;
use parser\error\CharacterError;
use parser\error\MatchError;
use parser\error\MatchErrors;
use parser\error\ParseException;
use parser\error\RawError;
use parser\error\WildcardError;
use parser\expression\CharClassExpression;
use parser\expression\Expression;
use parser\expression\ExpressionType;
use parser\expression\OptExpression;
use parser\expression\PlusExpression;
use parser\expression\SeqExpression;
use parser\expression\StarExpression;
use parser\result\Accepted;
use parser\result\MatchResult;
use parser\result\MatchResultType;
use parser\result\Rejected;
use RuntimeException;

class Parser
{
    private string $start;
    private string $input;
    private Automata $automata;

    public function __construct(ParserConfig $parserConfig)
    {
        $this->automata = $parserConfig->getAutomata();
        $this->start = $parserConfig->getStart();
    }

    /**
     * @return string
     */
    public function getStart(): string
    {
        return $this->start;
    }

    /**
     * @return Automata
     */
    public function getAutomata(): Automata
    {
        return $this->automata;
    }

    public function parse(string $input, string $start = null): AST
    {
        $this->input = $input;

        if (null !== $start) {
            $this->start = $start;
        }

        if (!array_key_exists($this->start, $this->automata->getArrayCopy())) {
            $keys = join(", ", array_keys($this->automata->getArrayCopy()));
            throw new RuntimeException("Invalid non-terminal $this->start. Expected one of $keys!");
        }

        return $this->match();
    }

    private function match(): AST
    {
        $action = $this->moveEval($this->evalNext($this->automata[$this->start]->getExpression(), 0, new ASTs(), new RawError(0, array(), new MatchErrors(), $this->start), new Continuations()));

        while (true) {
            switch ($action->getType()) {
                case ActionType::EVAL:
                {
                    $action = $this->moveEval(EvalAction::asEvalAction($action));
                    break;
                }
                case ActionType::APPLY:
                {
                    $action = ApplyAction::asApplyAction($action);
                    $continuations = $action->getContinuations();
                    $matchResult = $action->getMatchResult();

                    if ($continuations->isEmpty()) {
                        return $this->moveReturn($matchResult);
                    }

                    $action = $this->moveApply($matchResult, $continuations->head(), $continuations->tail());
                    break;
                }
                default:
                {
                    $type = $action->getType();
                    throw new RuntimeException("Unsupported action type $type!");
                }
            }
        }

        throw new RuntimeException("must not be here!");
    }

    private function moveEval(EvalAction $action): Action
    {
        $expression = $action->getExpression();
        $position = $action->getPosition();
        $asts = $action->getAsts();
        $error = $action->getError();
        $continuations = $action->getContinuations();
        $eof = $position >= strlen($this->input);

        switch ($expression->getType()) {
            case ExpressionType::ANY_CHAR:
            {
                if ($eof) {
                    $matchResult = $this->reject($this->updateError($error, $position, new WildcardError()));
                } else {
                    $matchResult = $this->accept($position + 1, ASTs::asts(new Char($this->input[$position], $position), $asts), $error);
                }

                return $this->applyNext($continuations, $matchResult);
            }
            case ExpressionType::ALT:
            {
                $expressions = Expression::asAltExpression($expression)->getExpressions();

                if ($expressions->isEmpty()) {
                    return $this->applyNext($continuations, $this->reject($error));
                } else {
                    $cons = Continuations::cons(new AltContinuation($expressions->tail(), $position, $asts), $continuations);
                    return $this->evalNext($expressions->head(), $position, $asts, $error, $cons);
                }
            }
            case ExpressionType:: AND:
            {
                $cons = Continuations::cons(new AndContinuation($position, $asts, $error), $continuations);
                return $this->evalNext($expression->getExpression(), $position, new ASTs(), $error, $cons);
            }
            case ExpressionType::NOT:
            {
                $cons = Continuations::cons(new NotContinuation($position, $asts, $error), $continuations);
                return $this->evalNext($expression->getExpression(), $position, new ASTs(), $error, $cons);
            }
            case ExpressionType::VOID:
            {
                $cons = Continuations::cons(new VoidContinuation($asts), $continuations);
                return $this->evalNext($expression->getExpression(), $position, new ASTs(), $error, $cons);
            }
            case ExpressionType::CHAR:
            {
                $char = Expression::asCharExpression($expression)->getChar();

                if ($eof) {
                    $matchResult = $this->reject($this->updateError($error, $position, new CharacterError($char)));
                } else {
                    $matches = $char === $this->input[$position];

                    if (!$matches) {
                        $matchResult = $this->reject($this->updateError($error, $position, new CharacterError($char)));
                    } else {
                        $matchResult = $this->accept($position + strlen($char), ASTs::asts(new Char($this->input[$position], $position), $asts), $error);
                    }
                }

                return $this->applyNext($continuations, $matchResult);
            }
            case ExpressionType::CHAR_CLASS:
            {
                $expression = CharClassExpression::asCharClassExpression($expression);

                if ($eof) {
                    $matchResult = $this->reject($this->updateError($error, $position, new CharacterClassError($expression)));
                } else {
                    $match = $this->matchCharClass($this->input[$position], $expression);


                    if ($match === true) {
                        $matchResult = $this->accept($position + 1, ASTs::asts(new Char($this->input[$position], $position), $asts), $error);
                    } else {
                        $matchResult = $this->reject($this->updateError($error, $position, new CharacterClassError($expression)));
                    }
                }

                return $this->applyNext($continuations, $matchResult);
            }
            case ExpressionType::SEQ:
            {
                $expressions = SeqExpression::asSeqExpression($expression)->getExpressions();

                if ($expressions->isEmpty()) {
                    return $this->applyNext($continuations, $this->accept($position, $asts, $error));
                } else {
                    $cons = Continuations::cons(new SeqContinuation($expressions->tail()), $continuations);
                    return $this->evalNext($expressions->head(), $position, $asts, $error, $cons);
                }
            }
            case ExpressionType::PLUS:
            {
                $cons = Continuations::cons(new PlusContinuation(PlusExpression::asPlusExpression($expression)->getExpression()), $continuations);
                return $this->evalNext(PlusExpression::asPlusExpression($expression)->getExpression(), $position, $asts, $error, $cons);
            }
            case ExpressionType::STAR:
            {
                $cons = Continuations::cons(new StarContinuation(StarExpression::asStarExpression($expression)->getExpression(), $position, $asts), $continuations);
                return $this->evalNext(StarExpression::asStarExpression($expression)->getExpression(), $position, $asts, $error, $cons);
            }
            case ExpressionType::OPT:
            {
                $cons = Continuations::cons(new OptContinuation($position, $asts), $continuations);
                return $this->evalNext(OptExpression::asOptExpression($expression)->getExpression(), $position, $asts, $error, $cons);
            }
            case ExpressionType::NT:
            {
                $expression = Expression::asNonTerminalExpression($expression);
                $name = $expression->getName();
                $automaton = $this->automata[$name];
                $cons = Continuations::cons(new NonTerminalContinuation($automaton->getMode(), $name, $asts, $error->getCurrentNonTerminal(), $position), $continuations);

                return $this->evalNext($automaton->getExpression(), $position, new ASTs(), new RawError($error->getPosition(), $error->getNonTerminals(), $error->getFailedChars(), $name), $cons);
            }
            default:
            {
                $type = $expression->getType();
                throw new RuntimeException("Unsupported expression type $type!");
            }
        }
    }

    private function moveApply(MatchResult $value, Continuation $evaluated, Continuations $rest): Action
    {
        switch ($value->getType()) {
            case MatchResultType::ACCEPTED:
            {
                return $this->moveApplyOnAccept(Accepted::asAccepted($value), $evaluated, $rest);
            }
            case MatchResultType::REJECTED:
            {
                return $this->moveApplyOnReject(Rejected::asRejected($value), $evaluated, $rest);
            }
            default:
            {
                throw new RuntimeException("Unsupported type of result $value.");
            }
        }
    }

    private function moveApplyOnAccept(Accepted $accepted, Continuation $evaluated, Continuations $rest): Action
    {
        switch ($evaluated->getType()) {
            case ContinuationType::SEQ:
            {
                $expressions = SeqContinuation::asSeqContinuation($evaluated)->getExpressions();
                if ($expressions->isEmpty()) {
                    return $this->applyNext($rest, $accepted);
                } else {
                    $cons = Continuations::cons(new SeqContinuation($expressions->tail()), $rest);
                    return $this->evalNext($expressions->head(), $accepted->getPosition(), $accepted->getAsts(), $accepted->getError(), $cons);
                }
                break;
            }
            case ContinuationType::STAR:
            case ContinuationType::PLUS:
            {
                $cons = Continuations::cons(new StarContinuation($evaluated->getExpression(), $accepted->getPosition(), $accepted->getAsts()), $rest);
                return $this->evalNext($evaluated->getExpression(), $accepted->getPosition(), $accepted->getAsts(), $accepted->getError(), $cons);
            }
            case ContinuationType::ALT:
            case ContinuationType::OPT:
            {
                return $this->applyNext($rest, $accepted);
            }
            case ContinuationType:: AND:
            {
                return $this->applyNext($rest, $this->accept($evaluated->getPosition(), $evaluated->getAsts(), $evaluated->getError()));
            }
            case ContinuationType::VOID:
            {
                return $this->applyNext($rest, $this->accept($accepted->getPosition(), $evaluated->getAsts(), $accepted->getError()));
            }
            case ContinuationType::NOT:
            {
                return $this->applyNext($rest, $this->reject($evaluated->getError()));
            }
            case ContinuationType::NT:
            {
                $evaluated = NonTerminalContinuation::asNTContinuation($evaluated);
                $mode = $evaluated->getMode();
                $name = $evaluated->getName();
                $asts = $evaluated->getAsts();
                $nonTerminal = $evaluated->getNonTerminal();
                $valAsts = $accepted->getAsts();
                $newError = new RawError($accepted->getError()->getPosition(), $accepted->getError()->getNonTerminals(), $accepted->getError()->getFailedChars(), $nonTerminal);

                switch ($mode) {
                    case NonTerminalMode::NORMAL:
                    {
                        return $this->applyNext($rest, $this->accept($accepted->getPosition(), ASTs::asts(new AST($name, $valAsts->reverse(), $evaluated->getStartPosition(), $accepted->getPosition()), $asts), $newError));
                    }
                    case NonTerminalMode::PRUNING:
                    {
                        if ($valAsts->isEmpty()) {
                            return $this->applyNext($rest, $this->accept($accepted->getPosition(), $asts, $newError));
                        } elseif ($valAsts->tail()->isEmpty()) {
                            return $this->applyNext($rest, $this->accept($accepted->getPosition(), ASTs::asts($valAsts->head(), $asts), $newError));
                        } else {
                            return $this->applyNext($rest, $this->accept($accepted->getPosition(), ASTs::asts(new AST($name, $valAsts->reverse(), $evaluated->getStartPosition(), $accepted->getPosition()), $asts), $newError));
                        }
                    }
                    case NonTerminalMode::VOIDING:
                    {
                        return $this->applyNext($rest, $this->accept($accepted->getPosition(), $asts, $newError));
                    }
                    default:
                    {
                        throw new RuntimeException("Unsupported mode in $evaluated.");
                    }
                }
            }
        }

        throw new RuntimeException("unexpected location");
    }

    private function moveApplyOnReject(Rejected $rejected, Continuation $evaluated, Continuations $continuations): Action
    {
        switch ($evaluated->getType()) {
            case ContinuationType::ALT:
            {
                $evaluated = AltContinuation::asAltContinuation($evaluated);
                $expressions = $evaluated->getExpressions();

                if ($expressions->isEmpty()) {
                    return $this->applyNext($continuations, $rejected);
                } else {
                    $cons = Continuations::cons(new AltContinuation($expressions->tail(), $evaluated->getPosition(), $evaluated->getAsts()), $continuations);
                    return $this->evalNext($expressions->head(), $evaluated->getPosition(), $evaluated->getAsts(), $rejected->getError(), $cons);
                }
            }
            case ContinuationType::SEQ:
            case ContinuationType::VOID:
            case ContinuationType::PLUS:
            {
                return $this->applyNext($continuations, $rejected);
            }
            case ContinuationType:: AND:
            {
                return $this->applyNext($continuations, $this->reject(AndContinuation::asAndContinuation($evaluated)->getError()));
            }
            case ContinuationType::NOT:
            case ContinuationType::STAR:
            case ContinuationType::OPT:
            {
                return $this->applyNext($continuations, $this->accept($evaluated->getPosition(), $evaluated->getAsts(), $rejected->getError()));
            }
            case ContinuationType::NT:
            {
                $error = $rejected->getError();
                $matchResult = $this->reject(new RawError($error->getPosition(), $error->getNonTerminals(), $error->getFailedChars(), $evaluated->getNonTerminal()));
                return $this->applyNext($continuations, $matchResult);
            }
            default:
            {
                throw new RuntimeException("Unsupported continuation type in $evaluated.");
            }
        }
    }

    private function moveReturn(MatchResult $matchResult): AST
    {
        switch ($matchResult->getType()) {
            case MatchResultType::ACCEPTED:
            {
                $matchResult = Accepted::asAccepted($matchResult);

                if ($matchResult->getPosition() >= strlen($this->input)) {
                    $automaton = $this->automata[$this->start];
                    $mode = $automaton->getMode();

                    switch ($mode) {
                        case NonTerminalMode::NORMAL:
                        {
                            return new AST($this->start, $matchResult->getAsts()->reverse(), 0, $matchResult->getPosition());
                        }
                        case NonTerminalMode::PRUNING:
                        {
                            if ($matchResult->getAsts()->isEmpty()) {
                                return new EmptyAST(0, $matchResult->getPosition());
                            } elseif ($matchResult->getAsts()->tail()->isEmpty()) {
                                $ast = $matchResult->getAsts()->head();

                                if (is_string($ast)) {
                                    throw new RuntimeException("Expected AST got string $ast");
                                } else {
                                    return $ast;
                                }
                            } else {
                                return new AST($this->start, $matchResult->getAsts()->reverse(), 0, $matchResult->getPosition());
                            }
                        }
                        case NonTerminalMode::VOIDING:
                        {
                            return new EmptyAST(0, $matchResult->getPosition());
                        }
                        default:
                        {
                            throw new RuntimeException("Unsupported mode from automaton $automaton.");
                        }
                    }
                } elseif ($matchResult->getError() && $matchResult->getPosition() === $matchResult->getError()->getPosition()) {
                    $error = new RawError($matchResult->getPosition(), $matchResult->getError()->getNonTerminals(), $matchResult->getError()->getFailedChars(), "");
                    throw new ParseException($error->toParseError($this->input));
                } else {
                    $error = new RawError($matchResult->getPosition(), array(), new MatchErrors(), "");
                    throw new ParseException($error->toParseError($this->input));
                }
                break;
            }
            case MatchResultType::REJECTED:
            {
                throw new ParseException($matchResult->getError()->toParseError($this->input));
            }
            default:
            {
                throw new RuntimeException("Unsupported type of matchResult $matchResult.");
            }
        }
    }

    private function updateError(RawError $error, int $position, MatchError $matchError): RawError
    {
        if (null === $error) {
            return new RawError($error->getPosition(), $error->getNonTerminals(), $error->getFailedChars(), $error->getCurrentNonTerminal());
        } else {
            if ($position > $error->getPosition()) {
                return new RawError($position, array($error->getCurrentNonTerminal()), new MatchErrors($matchError), $error->getCurrentNonTerminal());
            } elseif ($position === $error->getPosition()) {
                return new RawError($error->getPosition(), array_merge(array($error->getCurrentNonTerminal()), $error->getNonTerminals()), MatchErrors::matchErrors($matchError, $error->getFailedChars()), $error->getCurrentNonTerminal());
            } else {
                return new RawError($error->getPosition(), $error->getNonTerminals(), $error->getFailedChars(), $error->getCurrentNonTerminal());
            }
        }
    }

    private function evalNext(Expression $expression, int $position, ASTs $asts, RawError $rawError, Continuations $continuations): EvalAction
    {
        return new EvalAction($continuations, $expression, $position, $asts, $rawError);
    }

    private function applyNext(Continuations $continuations, MatchResult $matchResult): ApplyAction
    {
        return new ApplyAction($continuations, $matchResult);
    }

    private function accept(int $position, ASTs $asts, RawError $error): Accepted
    {
        return new Accepted($position, $asts, $error);
    }

    private function reject(RawError $error): Rejected
    {
        return new Rejected($error);
    }

    private function matchCharClass(string $char, CharClassExpression $expression): bool
    {
        $single = $expression->getSingle();
        $min = $expression->getMin();
        $max = $expression->getMax();

        $match = false;
        for ($i = 0; $i < count($min); $i++) {
            if ($char >= $min[$i] && $char <= $max[$i]) {
                $match = true;
                break;
            }
        }

        if (!$match) {
            for ($i = 0; $i < count($single); $i++) {
                if ($char === $single[$i]) {
                    $match = true;
                    break;
                }

            }
        }

        return $match;
    }
}
