package org.waxeye.parser;

/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Copyright (c) 2015 Joshua Gross
 * @author Damilare Akinlaja, 2017
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
class NonTerminal 
{
	public var mode:Modes;
	public var exp:Exp;

	public function new(mode:Modes, exp:Exp) 
	{
		this.mode = mode;
		this.exp = exp;
	}
	
	
	public static function nonterminal(mode:Modes, exp:Exp):NonTerminal
	{
		return new NonTerminal(mode, exp);
	}
	
}