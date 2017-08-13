package org.waxeye.parser;

/**
 * ...
 * @author Damilare Akinlaja
 * Based on https://github.com/orlandohill/waxeye/blob/master/src/javascript/waxeye.coffee
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