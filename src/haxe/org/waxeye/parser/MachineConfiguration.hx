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
class MachineConfiguration
{
	public var type:String;
	public var exp:Exp;
	public var pos:Int;
	public var asts:Array<AST>;
	public var err:Dynamic;
	public var continuations:Array<Continuations>;
	public var value:Value;

	public function new(type:String, exp:Exp, pos:Int, asts:Array<AST>, err:Dynamic, continuations:Array<Continuations>, value:Value)
	{
		this.type = type;
		this.exp = exp;
		this.pos = pos;
		this.asts = asts;
		this.err = err;
		this.continuations = continuations;
		this.value = value;
	}
	
	
	public static function EVAL(exp:Exp, pos:Int, asts:Array<AST>, err:Dynamic, continuations:Array<Continuations>):MachineConfiguration
	{
		return new MachineConfiguration("EVAL", exp, pos, asts, err, continuations, null);
	}

	
	public static function APPLY(continuations:Array<Continuations>, value:Value):MachineConfiguration
	{
		return new MachineConfiguration("APPLY", null, 0, null, null, continuations, value);
	}	
}