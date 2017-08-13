package org.waxeye.parser;
import haxe.DynamicAccess;
import org.waxeye.parser.Exp.ExpType;
import org.waxeye.parser.Util.Assert;
import haxe.ds.StringMap;

/**
 * ...
 * @author Damilare Akinlaja
 */
class Parser
{
	public var env:DynamicAccess<Dynamic>;
	public var start:String;

	private var inputLen:Int;
	private var input:String;
	private var nt:String;
	
	private var state:MachineState;

	public function new(env:DynamicAccess<Dynamic>, start:String = '')
	{

		if (env == null)
		{
			throw "Please supply grammar definition.";
		}

		this.env = env;

		this.start = start;
	}

	public function match(nt:String, input:String):Dynamic
	{
		this.input = input;
		this.nt = nt;
		this.inputLen = input.length;

		//move from initial state to halting state
		this.move(MachineConfiguration.EVAL(this.env.get(nt).exp, 0, [], new RawError(0, [nt], [], nt), []));
		if (state != null)
		{
			while (state.type != "FINAL")
			{
				this.move(state.configuration);
			}
		}
		return state.result;

		//return {};

	}

	private function eof(pos:Int):Bool
	{
		return pos >= inputLen;
	}

	//Move configuration -> state
	private function move(conf:MachineConfiguration):Void
	{

		var asts:Array<AST> = new Array<AST>();
		if (conf.asts == null)
		{
			asts = [];
		}
		else{
			asts = conf.asts;
		}
		var pos:Int = conf.pos;
		var exp:Exp = conf.exp;
		var err:Dynamic = conf.err;

		//trace(asts);

		var k:Array<Continuations> = conf.continuations;
		var firstExp:Exp = new Exp(null, []);
		var restExp:Array<Exp>= new Array<Exp>();
		var es:Array<Dynamic> = [];

		var kFirst = Util.first(k);
		var kRest = Util.rest(k);
		if (kFirst != null)
			es = kFirst.expressions;

		if (es != null)
		{
			firstExp = Util.first(es);
			restExp = Util.rest(es);
		}

		
		switch conf.type {

			case "EVAL": {

				switch exp.type
				{
					case ExpType.ANY:
						{

							if (eof(pos))
								{
									this.state = MachineState.INTER(MachineConfiguration.APPLY(k, Value.FAIL(Util.updateError(err, pos, new ErrAny()))));

								}
								else{
									this.state = MachineState.INTER(MachineConfiguration.APPLY(k, Value.VAL(pos + 1, Util.arrayPrepend(input.charAt(pos), asts), err)));

								}

							};
						case ExpType.ALT:
							{
								es = exp.args;
								if (es.length > 0)
								{
									this.state = MachineState.INTER(MachineConfiguration.EVAL(Util.first(es), pos, asts, err, Util.arrayPrepend(Continuations.CONT_ALT(Util.rest(es), pos, asts), k)));

								}
								else{
									this.state = MachineState.INTER(MachineConfiguration.APPLY(k, err));

								}
							};
						case ExpType.AND:
							{
								this.state = MachineState.INTER(MachineConfiguration.EVAL(exp.args[0], pos, [], err, Util.arrayPrepend(Continuations.CONT_AND(pos, asts, err), k)));

							};
						case ExpType.NOT:
							{
								this.state = MachineState.INTER(MachineConfiguration.EVAL(exp.args[0], pos, [], err, Util.arrayPrepend(Continuations.CONT_NOT(pos, asts, err), k)));

							};
						case ExpType.VOID:
							{
								this.state = MachineState.INTER(MachineConfiguration.EVAL(exp.args[0], pos, [], err, Util.arrayPrepend(Continuations.CONT_VOID(asts), k)));

							};
						case ExpType.CHAR:
							{
								var c = exp.args[0];
								var newval;
								if (eof(pos) || c != input.charAt(pos))
								{
									newval = Value.FAIL(Util.updateError(err, pos, new ErrChar(c)));
								}
								else{
									newval = Value.VAL(pos + 1, Util.arrayPrepend(input.charAt(pos), asts), err);
								}
								this.state = MachineState.INTER(MachineConfiguration.APPLY(k, newval));

							};
						case ExpType.CHAR_CLASS:
							{
								var cc = exp.args;
								
								if (eof(pos))
								{
									this.state = MachineState.INTER(MachineConfiguration.APPLY(k, Value.FAIL(Util.updateError(err, pos, new ErrCC(cc)))));

								}
								else{
									this.state = visit(cc, k, err, pos, cc, asts);
								}
							};
						case ExpType.SEQ:{
								/** a sequence is made up of a list of expressions
								 * we traverse the list, making sure each expression succeeds
								 * the rest of the string return by the expression is used
								 * as input to the next expression
								*/
								var exprs:Array<Dynamic> = exp.args;
								if (exprs == null)
								{
									this.state = MachineState.INTER(MachineConfiguration.APPLY(k, Value.VAL(pos, asts, err)));
								}
								else{
									this.state = MachineState.INTER(MachineConfiguration.EVAL(Util.first(exprs), pos, asts, err, Util.arrayPrepend(Continuations.CONT_SEQ(Util.rest(exprs)), k)));
								}

							};
						case ExpType.PLUS:{
								this.state = MachineState.INTER(MachineConfiguration.EVAL(exp.args[0], pos, asts, err, Util.arrayPrepend(Continuations.CONT_PLUS(exp.args[0]), k)));

							};
						case ExpType.STAR:{
								this.state = MachineState.INTER(MachineConfiguration.EVAL(exp.args[0], pos, asts, err, Util.arrayPrepend(Continuations.CONT_STAR(exp.args[0], pos, asts), k)));

							};
						case ExpType.OPT:{
								this.state = MachineState.INTER(MachineConfiguration.EVAL(exp.args[0], pos, asts, err, Util.arrayPrepend(Continuations.CONT_OPT(pos, asts), k)));

							};
						case ExpType.NT:{
								var name:String = exp.args[0];
								var mode:Modes = this.env.get(name).mode;
								var e:Exp = this.env.get(name).exp;
								var err:RawError = new RawError(err.pos, err.nonterminals, err.failedChars, name);

								this.state = MachineState.INTER(MachineConfiguration.EVAL(e, pos, [], err, Util.arrayPrepend(Continuations.CONT_NT(mode, name, asts, conf.err.currentNT), k)));

							};
						default: throw "unsupported 2";
					}

				};

			case "APPLY":{
				    
						if ((conf.value != null && conf.value.type == "FAIL") && (kFirst != null && ["CONT_ALT"].indexOf(kFirst.type) == -1))
						{
							if (["CONT_SEQ", "CONT_VOID", "CONT_PLUS"].indexOf(kFirst.type) != -1)
							{
								this.state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), conf.value));

							}
							else if (["CONT_AND"].indexOf(kFirst.type) != -1)
							{
								this.state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), Value.FAIL(cast(kFirst.err, RawError))));

							}
							else if (["CONT_NOT"].indexOf(kFirst.type) != -1)
							{
								this.state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), Value.VAL(kFirst.pos, kFirst.asts, cast(conf.value.err, RawError))));

							}
							else if (["CONT_STAR", "CONT_OPT"].indexOf(kFirst.type) != -1)
							{
								this.state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), Value.VAL(kFirst.pos, kFirst.asts, cast(conf.value.err, RawError))));

							}
							else if (["CONT_NT"].indexOf(kFirst.type) != -1)
							{
								err = conf.value.err;
								this.state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), Value.FAIL(new RawError(err.pos, err.nonterminals, err.failedChars, kFirst.nt))));

							}
							else
							{
								this.state = MachineState.FINAL(cast(conf.value.err, RawError).toParseError(input));

							}
						}
						else if (kFirst!= null && kFirst.type == "CONT_SEQ")
						{
							if (es.length > 0)
							{
								this.state = MachineState.INTER(MachineConfiguration.EVAL(firstExp, conf.value.pos, conf.value.asts, cast(conf.value.err, RawError), Util.arrayPrepend(Continuations.CONT_SEQ(restExp), kRest)));

							}
							else
							{
								this.state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), conf.value));

							}

						}

						//The second continuation used to evaluate PLUS
						//is the same continuation as for STAR
						else if (kFirst != null && (kFirst.type == "CONT_STAR" || kFirst.type == "CONT_PLUS"))
						{
							Assert.ok(Std.is(conf.value, Value));
							this.state = MachineState.INTER(MachineConfiguration.EVAL(kFirst.expression, conf.value.pos, conf.value.asts, cast(conf.value.err, RawError), Util.arrayPrepend(Continuations.CONT_STAR(kFirst.expression, conf.value.pos, conf.value.asts), kRest)));

						}
						else if (kFirst != null && kFirst.type == "CONT_VOID")
						{
							this.state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), Value.VAL(conf.value.pos, kFirst.asts, cast(conf.value.err, RawError))));

						}
						else if (kFirst != null && kFirst.type == "CONT_ALT")
						{

							if (conf.value.type == "FAIL" && es.length > 0)
							{
								this.state = MachineState.INTER(MachineConfiguration.EVAL(Util.first(es), kFirst.pos, kFirst.asts,  cast(conf.value.err, RawError), Util.arrayPrepend(Continuations.CONT_ALT(Util.rest(es), kFirst.pos, kFirst.asts), kRest)));

							}
							else
							{
								this.state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), conf.value));

							}
						}
						else if (kFirst != null && kFirst.type == "CONT_OPT")
						{
							this.state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), conf.value));

						}
						else if (kFirst != null && kFirst.type == "CONT_AND")
						{
							this.state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), Value.VAL(kFirst.pos, kFirst.asts, kFirst.err)));

						}
						else if (kFirst != null && kFirst.type == "CONT_NOT")
						{
							this.state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), Value.FAIL(kFirst.err)));

						}
						else if (kFirst != null && kFirst.type == "CONT_NT")
						{
							var mode = kFirst.mode;
							var name = kFirst.name;
							var asts = kFirst.asts;
							var nt = kFirst.nt;

							var value:Value = conf.value;
							var valAsts:Array<AST> = value.asts;
							var errPos:Int = cast(value.err, RawError).pos;
							var errNts:Array<Dynamic> = cast(value.err, RawError).nonterminals;
							var errCcs:Array<Dynamic> = cast(value.err, RawError).failedChars;

							var newErr = new RawError(errPos, errNts, errCcs, nt);

							switch mode
							{
								case Modes.NORMAL:{
									valAsts.reverse();
										this.state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), Value.VAL(value.pos, Util.arrayPrepend(AST.TREE(name, valAsts), asts), newErr)));

									};
								case Modes.PRUNING:{
										if (valAsts.length == 0)
										{
											this.state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), Value.VAL(value.pos, asts, newErr)));

										}
										else if (valAsts.length == 1)
										{
											this.state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), Value.VAL(value.pos, Util.arrayPrepend(Util.first(valAsts), asts), newErr)));

										}
										else{
											valAsts.reverse();
											this.state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), Value.VAL(value.pos, Util.arrayPrepend(AST.TREE(name, valAsts), asts), newErr)));

										}
									};
								case Modes.VOIDING:{
										this.state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), Value.VAL(value.pos, asts, newErr)));

									};
							}
						}
						else if (kFirst != null)
						{
							throw 'unsupported 4';
						}
						else if (conf.value.type == "VAL")
						{
							var ts:Array<AST> = conf.value.asts;

							if (eof(conf.value.pos))
							{
								if (this.env.get(this.start).mode == Modes.NORMAL)
								{
									ts.reverse();
									this.state = MachineState.FINAL(AST.TREE(this.start, ts));

								}
								else if (this.env.get(this.start).mode == Modes.PRUNING)
								{

									if (ts.length == 0)
									{
										this.state = MachineState.FINAL(AST.EMPTY());

									}
									else if (ts.length == 1)
									{
										this.state = MachineState.FINAL(Util.first(ts));

									}
									else
									{
										ts.reverse();
										this.state = MachineState.FINAL(AST.TREE(this.start, ts));

									}
								}
								else
								{
									this.state = MachineState.FINAL(AST.EMPTY());
								}
							}
							else if (cast(conf.value.err, RawError) != null && conf.value.pos == cast(conf.value.err, RawError).pos)
							{
								var err:RawError = conf.value.err;
								this.state = MachineState.FINAL((new RawError(conf.value.pos, err.nonterminals, err.failedChars)).toParseError(input));

							}
							else
							{
								this.state = MachineState.FINAL((new RawError(conf.value.pos, [], [])).toParseError(input));

							}
						}
						else if (conf.value.type == "FAIL")
						{
							this.state = MachineState.FINAL(cast(conf.value.err, RawError).toParseError(input));

						}
						else
						{
							throw 'unsupported 3';
						}

				};
				default: 

		}

	}

	private function visit(charClasses:Dynamic, k, err, pos, cc, asts):MachineState
	{
		var c1 = null;
		var c2 = null;

		if (charClasses.length == 0)
		{
			return MachineState.INTER(MachineConfiguration.APPLY(k, Value.FAIL(Util.updateError(err, pos, new ErrCC(cc)))));
		}
		else
		{
			if (Std.is(Util.first(charClasses), Array))
			{
				c1 = Util.first(charClasses)[0];
				c2 = Util.first(charClasses)[1];
			}
			else
			{
				c1 = c2 = Util.first(charClasses);
			}
		}

		if (c1 <= input.charAt(pos) && c2 >= input.charAt(pos))
		{
			return MachineState.INTER(MachineConfiguration.APPLY(k, Value.VAL(pos + 1, Util.arrayPrepend(input.charAt(pos), asts), err)));
		}
		
		return visit(Util.rest(charClasses), k, err, pos, cc, asts);
		
	}

	public function parse(input):Dynamic
	{
		return this.match(this.start, input);
	}

}