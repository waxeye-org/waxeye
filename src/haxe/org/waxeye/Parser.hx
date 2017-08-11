package org.waxeye;
import org.waxeye.Exp.ExpType;
import org.waxeye.Util.Assert;
import haxe.DynamicAccess;

/**
 * ...
 * @author Damilare Akinlaja
 */
class Parser
{
	public var env:DynamicAccess<Dynamic>;
	public var start:String;

	public function new(env:DynamicAccess<Dynamic>, start:String = '')
	{
		
		if(this.env.keys().length == 0){
			this.env[" "] = "";
		}
		

		
		this.start = start;
	}

	public function match(nt:String, input:String):Dynamic
	{
		var inputLen:Int = input.length;
		var eof:Int->Bool = function(pos:Int):Bool
		{
			return pos >= inputLen;
		}
		//Move configuration -> state
		var move:MachineConfiguration->MachineState = function (conf:MachineConfiguration):MachineState
		{
			var asts:Array<AST> = conf.asts;
			var pos:Int = conf.pos;
			var exp:Exp = conf.exp;
			var err:Dynamic = conf.err;

			var k:Array<Continuations> = conf.continuations;

			var kFirst:Continuations = new Continuations(null, 0, null, null, null, null, null, null, null); 
			var kRest:Array<Dynamic> = new Array<Dynamic>();
			var firstExp:Exp = new Exp(null, []);
			var restExp:Array<Exp>= new Array<Exp>();
			var es:Array<Exp> = [];

			if (k != null)
			{
				kFirst = cast(Util.first(k),Continuations);
				kRest = Util.rest(k);

				if (kFirst != null)
				{
					es = kFirst.expressions;

					if (es != null)
					{
						firstExp = Util.first(es);
						restExp = Util.rest(es);
					}
				}

			}
			
			var _state:MachineState = MachineState.INTER(null);

			switch conf.type {
				
				case "EVAL": {

					switch exp.type
					{
						case ExpType.ANY:
								{

									if (eof(pos)){
										_state =  MachineState.INTER(MachineConfiguration.APPLY(k, Value.FAIL(Util.updateError(err, pos, new ErrAny()))));
									}
									else{
										_state = MachineState.INTER(MachineConfiguration.APPLY(k, Value.VAL(pos + 1, Util.arrayPrepend(input.charAt(pos), asts), err)));
									}

								};
							case ExpType.ALT:
								{
									es = cast exp.args;
									if (es.length > 0)
									{
										_state = MachineState.INTER(MachineConfiguration.EVAL(Util.first(es), pos, asts, err, Util.arrayPrepend(Continuations.CONT_ALT(Util.rest(es), pos, asts), k)));
									}
									else{
										_state = MachineState.INTER(MachineConfiguration.APPLY(k, err));
									}
								};
							case ExpType.AND:
								{
									_state = MachineState.INTER(MachineConfiguration.EVAL(exp.args[0], pos, [], err, Util.arrayPrepend(Continuations.CONT_AND(pos, asts, err), k)));
								};
							case ExpType.NOT:
								{
									_state = MachineState.INTER(MachineConfiguration.EVAL(exp.args[0], pos, [], err, Util.arrayPrepend(Continuations.CONT_NOT(pos, asts, err), k)));
								};
							case ExpType.VOID:
								{
									_state = MachineState.INTER(MachineConfiguration.EVAL(exp.args[0], pos, [], err, Util.arrayPrepend(Continuations.CONT_VOID(asts), k)));
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
									_state = MachineState.INTER(MachineConfiguration.APPLY(k, newval));
								};
							case ExpType.CHAR_CLASS:
								{
									var cc = exp.args;
									var c1:String = null;
									var c2:String = null;
									var visit:Any->MachineState = function(charClasses:Any):MachineState{
										return _state;
									};
									
									visit = function(charClasses:Any){
										var __state = _state;
										if (cast(charClasses, Array<Dynamic>).length == 0){
											__state =  MachineState.INTER(MachineConfiguration.APPLY(k, Value.FAIL(Util.updateError(err, pos, new ErrCC(cc)))));
										}else{
												c1 = (Std.is(Util.first(charClasses), Array) ? cast(Util.first(charClasses), Array<Dynamic>)[0] : Util.first(charClasses));
												c2 = (Std.is(Util.first(charClasses), Array) ? cast(Util.first(charClasses), Array<Dynamic>)[1] : Util.first(charClasses));
												
												if(c1 <= input.charAt(pos) && c2 >= input.charAt(pos)){
													__state = MachineState.INTER(MachineConfiguration.APPLY(k, Value.VAL(pos + 1, Util.arrayPrepend(input.charAt(pos), asts), err)));
												}else{
													__state =  cast(visit(Util.rest(charClasses)), MachineState);
												}												
										}
										
										return __state;
										
									}
									
									if(eof(pos)){
										_state = MachineState.INTER(MachineConfiguration.APPLY(k, Value.FAIL(Util.updateError(err, pos, new ErrCC(cc)))));
									}else{
										return visit(cc);
									}
								};
							case ExpType.SEQ:{
									/** a sequence is made up of a list of expressions
									 * we traverse the list, making sure each expression succeeds
									 * the rest of the string return by the expression is used
									 * as input to the next expression	
									*/
									var exprs:Array<Any> = exp.args;
									if (exprs == null)
									  _state = MachineState.INTER(MachineConfiguration.APPLY(k, Value.VAL(pos, asts, err)));
									else
									  _state = MachineState.INTER(MachineConfiguration.EVAL(Util.first(exprs), pos, asts, err, Util.arrayPrepend(Continuations.CONT_SEQ(Util.rest(exprs)), k)));									
							};
							case ExpType.PLUS:{
									_state = MachineState.INTER(MachineConfiguration.EVAL(exp.args[0], pos, asts, err, Util.arrayPrepend(Continuations.CONT_PLUS(exp.args[0]), k)));
							};
							case ExpType.STAR:{
									_state = MachineState.INTER(MachineConfiguration.EVAL(exp.args[0], pos, asts, err, Util.arrayPrepend(Continuations.CONT_STAR(exp.args[0], pos, asts), k)));
							};
							case ExpType.OPT:{
									_state = MachineState.INTER(MachineConfiguration.EVAL(exp.args[0], pos, asts, err, Util.arrayPrepend(Continuations.CONT_OPT(pos, asts), k)));
							};
							case ExpType.NT:{
									var name:String = exp.args[0];
									var mode:Modes = cast(this.env.get(name), NonTerminal).mode;
									var e:Exp = cast(this.env.get(name), NonTerminal).exp;
									var err:RawError = new RawError(err.pos, err.nonterminals, err.failedChars, name);
									
									_state = MachineState.INTER(MachineConfiguration.EVAL(e, pos, [], err, Util.arrayPrepend(Continuations.CONT_NT(mode, name, asts, cast(conf.value.err, RawError)), k)));
							};
							default: throw "unsupported 2";
					}
					
				};
				
				case "APPLY":{
					
					if (conf.value.type == "FAIL" && ["CONT_ALT"].indexOf(kFirst.type) == -1){
						if(["CONT_SEQ", "CONT_VOID", "CONT_PLUS"].indexOf(kFirst.type) != -1){
							_state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), conf.value));
						}else if(["CONT_AND"].indexOf(kFirst.type) != -1){
							_state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), Value.FAIL(cast(kFirst.err, RawError))));
						}else if (["CONT_NOT"].indexOf(kFirst.type) != -1){
							_state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), Value.VAL(kFirst.pos, kFirst.asts, cast(conf.value.err, RawError))));
						}else if(["CONT_STAR", "CONT_OPT"].indexOf(kFirst.type) != -1){
							_state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), Value.VAL(kFirst.pos, kFirst.asts, cast(conf.value.err, RawError))));
						}else if(["CONT_NT"].indexOf(kFirst.type) != -1){
							err = conf.value.err;
							_state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), Value.FAIL(new RawError(err.pos, err.nonterminals, err.failedChars, kFirst.nt))));
						}else{
							_state = MachineState.FINAL(cast(conf.value.err, RawError).toParseError(input));
						}
					}else if(kFirst.type == "CONT_SEQ"){
						if (es.length > 0){
							_state = MachineState.INTER(MachineConfiguration.EVAL(firstExp, conf.value.pos, conf.value.asts, cast(conf.value.err, RawError), Util.arrayPrepend(Continuations.CONT_SEQ(restExp), kRest)));
						}else{
							_state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), conf.value));
						}
						
					}
					
					//The second continuation used to evaluate PLUS
					//is the same continuation as for STAR
					else if (kFirst.type == "CONT_STAR" || kFirst.type == "CONT_PLUS"){
						Assert.ok(Std.is(conf.value, Value));
						_state = MachineState.INTER(MachineConfiguration.EVAL(kFirst.expression, conf.value.pos, conf.value.asts, cast(conf.value.err, RawError), Util.arrayPrepend(Continuations.CONT_STAR(kFirst.expression, conf.value.pos, conf.value.asts), kRest)));
					}
					else if (kFirst.type == "CONT_VOID"){
						_state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), Value.VAL(conf.value.pos, kFirst.asts, cast(conf.value.err, RawError))));
					}
					else if (kFirst.type == "CONT_ALT"){
						
						if(conf.value.type == "FAIL" && es.length > 0){
							_state = MachineState.INTER(MachineConfiguration.EVAL(Util.first(es), kFirst.pos, kFirst.asts,  cast(conf.value.err, RawError), Util.arrayPrepend(Continuations.CONT_ALT(Util.rest(es), kFirst.pos, kFirst.asts), kRest)));
						}else{
							_state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), conf.value));
						}
					}else if (kFirst.type == "CONT_OPT"){
						 _state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), conf.value));
					}else if (kFirst.type == "CONT_AND"){
						 _state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), Value.VAL(kFirst.pos, kFirst.asts, kFirst.err)));
					}else if (kFirst.type == "CONT_NOT"){
						 _state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), Value.FAIL(kFirst.err)));
					}else if (kFirst.type == "CONT_NT"){
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
						
						switch mode{
							case Modes.NORMAL:{
								valAsts.reverse();
								_state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), Value.VAL(value.pos, Util.arrayPrepend(AST.TREE(name, valAsts), asts), newErr)));
							};
							case Modes.PRUNING:{
							  if(valAsts.length == 0)
									_state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), Value.VAL(value.pos, asts, newErr)));
							  else if (valAsts.length == 1)
									_state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), Value.VAL(value.pos, Util.arrayPrepend(Util.first(valAsts), asts), newErr)));
							  else
									valAsts.reverse();
									_state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), Value.VAL(value.pos, Util.arrayPrepend(AST.TREE(name, valAsts), asts), newErr)));
							};
							case Modes.VOIDING:{
								_state = MachineState.INTER(MachineConfiguration.APPLY(cast(kRest), Value.VAL(value.pos, asts, newErr)));							
							};
						}
					}else if(kFirst != null){
						throw 'unsupported 4';
					}else if (conf.value.type == "VAL"){
						var ts:Array<AST> = conf.value.asts;
						
						if (eof(conf.value.pos)){
							if (this.env.get(this.start).mode == Modes.NORMAL){
								ts.reverse();
								_state = MachineState.FINAL(AST.TREE(this.start, ts));
							}else if(this.env.get(this.start).mode == Modes.PRUNING){
								
								if(ts.length == 0){
									_state = MachineState.FINAL(AST.EMPTY());
								}else if(ts.length == 1){
									_state = MachineState.FINAL(Util.first(ts));
								}else{
									ts.reverse();
									_state = MachineState.FINAL(AST.TREE(this.start, ts));
								}
							}else{
								MachineState.FINAL(AST.EMPTY());
							}
						}else if(cast(conf.value.err, RawError) != null && conf.value.pos == cast(conf.value.err, RawError).pos){
							var err:RawError = conf.value.err;
							_state = MachineState.FINAL((new RawError(conf.value.pos, err.nonterminals, err.failedChars)).toParseError(input));
						}else{
							_state = MachineState.FINAL((new RawError(conf.value.pos, [], [])).toParseError(input));
						}
					}else if(conf.value.type == "FAIL"){
						_state = MachineState.FINAL(cast(conf.value.err, RawError).toParseError(input));
					}else{
						throw 'unsupported 3';
					}
					
				};
				
				
			}
			
			
			return _state;

		}
		
		//move from initial state to halting state
		var state;
		if(this.env.exists(nt)){
			state = move(MachineConfiguration.EVAL(cast(this.env.get(nt), NonTerminal).exp, 0, [], new RawError(0, [nt], [], nt), []));
			while(state.type != "FINAL"){
				state = move(state.configuration);
			}
			return state.result;
		}else{
			throw "Parse Failed: You may have not properly set required env for your grammar";
		}
	}
	
	
	public function parse(input){
		return this.match(this.start, input);
	}

}