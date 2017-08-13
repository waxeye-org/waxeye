package org.waxeye.parser;

/**
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Copyright (c) 2015 Joshua Gross
 * @author Damilare Akinlaja, 2017
 * Licensed under the MIT license. See 'LICENSE' for details.
 *
  * An abstract syntax tree has one of three forms.
 * AST_EMPTY represents a successful parse from a voided non-terminal.
 * 'x' just holds a character.
 * AST_TREE represents a successful parse from a non-terminal. It holds:
 * - the non-terminal's name
 * - a list of child asts
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