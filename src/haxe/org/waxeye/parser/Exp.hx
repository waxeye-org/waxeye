package org.waxeye.parser;

/**
 * ...
 * @author Damilare Akinlaja
 * Based on https://github.com/orlandohill/waxeye/blob/master/src/javascript/waxeye.coffee
 */

@:enum
abstract ExpType(String){
	var ANY = 			"ANY";
	var NT = 			"NT";
	var VOID = 			"VOID";
	var CHAR = 			"CHAR";
	var CHAR_CLASS = 	"CHAR_CLASS";
	var AND = 			"AND";
	var NOT = 			"NOT";
	var OPT = 			"OPT";
	var ALT = 			"ALT";
	var SEQ = 			"SEQ";
	var STAR =			"STAR";
	var PLUS = 			"PLUS";
}
 
class Exp
{
	public var type:ExpType;
	public var args:Array<Dynamic>;
	
	public function new(type:ExpType, args:Array<Dynamic>) 
	{
		this.args = args;
		this.type = type;
	}
	
	
	public static function ANY(args:Array<Any>):Exp
	{
		return new Exp(ExpType.ANY, args);
	}
	
	public static function NT(args:Array<Any>):Exp
	{
		return new Exp(ExpType.NT, args);
	}
	
	public static function VOID(args:Array<Any>):Exp
	{
		return new Exp(ExpType.VOID, args);
	}
	
	public static function CHAR(args:Array<Any>):Exp
	{
		return new Exp(ExpType.CHAR, args);
	}
	
	public static function CHAR_CLASS(args:Array<Any>):Exp
	{
		return new Exp(ExpType.CHAR_CLASS, args);
	}
	
	public static function AND(args:Array<Any>):Exp
	{
		return new Exp(ExpType.AND, args);
	}
	
	public static function NOT(args:Array<Any>):Exp
	{
		return new Exp(ExpType.NOT, args);
	}
	
	public static function OPT(args:Array<Any>):Exp
	{
		return new Exp(ExpType.OPT, args);
	}
	
	public static function ALT(args:Array<Any>):Exp
	{
		return new Exp(ExpType.ALT, args);
	}
	
	public static function SEQ(args:Array<Any>):Exp
	{
		return new Exp(ExpType.SEQ, args);
	}
	
	public static function STAR(args:Array<Any>):Exp
	{
		return new Exp(ExpType.STAR, args);
	}
	
	public static function PLUS(args:Array<Any>):Exp
	{
		return new Exp(ExpType.PLUS, args);
	}	
	
}