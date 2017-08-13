package org.waxeye.parser;
import org.waxeye.parser.Modes.Modes;
import org.waxeye.parser.Util.Assert;
/**
 * ...
 * @author Damilare Akinlaja
 */
class Continuations 
{
	public var type:String;
	public var pos:Int;
	public var expressions:Array<Exp>;
	public var expression:Exp;
	public var asts:Array<AST>;
	public var err:Any;
	public var mode:Modes;
	public var name:String;
	public var nt:Dynamic;

	public function new(type:String, pos:Int, expressions:Array<Exp>, expression:Exp, asts:Array<AST>, err:Any, mode:Modes, name:String, nt:Dynamic) 
	{
		this.type = type;
		this.pos = pos;
		this.expressions = expressions;
		this.expression = expression;
		this.asts = asts;
		this.err = err;
		this.mode = mode;
		this.name = name;
		this.nt = nt;
		//Assert.ok(Std.is(asts, Array) || asts != null);
	}
	
	public static function CONT_SEQ(expressions:Array<Exp>):Continuations
	{
		return new Continuations("CONT_SEQ", 0, expressions, null, null, null, null, null, null);
	}
	
	public static function CONT_ALT(expressions: Array<Exp>, pos:Int, asts:Array<AST>):Continuations
	{
		return new Continuations("CONT_ALT", pos, expressions, null, asts, null, null, null, null);
	}
	
	public static function CONT_AND(pos:Int, asts:Array<AST>, err:Any):Continuations
	{
		return new Continuations("CONT_AND", pos, null, null, asts, err, null, null, null);
	}	
	
	public static function CONT_NOT( pos:Int, asts:Array<AST>, err:Any):Continuations
	{
		return new Continuations("CONT_NOT", pos, null, null, asts, err, null, null, null);
	}

	public static function CONT_OPT(pos:Int, asts:Array<AST>):Continuations
	{
		return new Continuations("CONT_OPT", pos, null, null, asts, null, null, null, null);
	}

	public static function CONT_STAR(exp: Dynamic, pos:Int, asts:Array<AST>):Continuations
	{
		return new Continuations("CONT_STAR", pos, null, exp, asts, null, null, null, null);
	}

	public static function CONT_PLUS(exp: Dynamic):Continuations
	{
		return new Continuations("CONT_PLUS", 0, null, exp, null, null, null, null, null);
	}

	public static function CONT_VOID(asts:Array<AST>):Continuations
	{
		return new Continuations("CONT_VOID", 0, null, null, asts, null, null, null, null);
	}
	
	public static function CONT_NT(mode:Modes, name:String, asts:Array<AST>, nt:Dynamic):Continuations
	{
		return new Continuations("CONT_NT", 0, null, null, asts, null, mode, name, nt);
	}	
	
}