package org.waxeye.parser;
import org.waxeye.parser.Util.Assert;

/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Copyright (c) 2015 Joshua Gross
 * @author Damilare Akinlaja, 2017
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

class Value
{
	public var type:String;
	public var err:Any;
	public var pos:Int;
	public var asts:Array<AST>;

	public function new(type:String, ?err:Any, ?pos:Int, ?asts:Array<AST>)
	{
		this.type = type;
		this.err = err;
		this.pos = pos;
		this.asts = asts;

	}
	
	public static function FAIL(err:Any):Value
	{
		return new Value("FAIL", err);
	}
	
	public static function VAL(pos:Int, asts:Array<AST>, err:Any):Value
	{
		return new Value("VAL", err, pos, asts);
	}

}