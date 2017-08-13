package org.waxeye.parser;

/**
 * An abstract syntax tree has one of three forms.
 * AST_EMPTY represents a successful parse from a voided non-terminal.
 * 'x' just holds a character.
 * AST_TREE represents a successful parse from a non-terminal. It holds:
 * - the non-terminal's name
 * - a list of child asts
 * 
 * @author Damilare Akinlaja
 * 
 * Based on https://github.com/orlandohill/waxeye/blob/master/src/javascript/waxeye.coffee
 * 
 */
class AST 
{

	public var form:String;
	public var type:Any;
	public var children:Array<Any>;
	
	public function new(form:String, ?type:Any, ?children:Any) 
	{
		this.form = form;
		this.type = type;
		this.children = children;
	}
	
	public static function EMPTY():AST
	{
		return new AST("EMPTY");
	}
	
	public static function TREE(str:Any, asts:Any):AST
	{
		return new AST("TREE", str, asts);
	}
	
}