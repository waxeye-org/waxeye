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
class RawError 
{
	public var pos:Int;
	public var nonterminals:Array<Dynamic>;
	public var failedChars:Array<Dynamic>;
	public var currentNT:Dynamic;

	public function new(pos:Int, nonterminals:Array<Dynamic>, failedChars:Array<Dynamic>, ?currentNT:Dynamic) 
	{
		this.pos = pos;
		this.nonterminals = nonterminals;
		this.failedChars = failedChars;
		this.currentNT = currentNT;
		
		Assert.ok(Std.is(this.nonterminals, Array));
		Assert.ok(Std.is(this.failedChars, Array));
	}
	
	public function toParseError(input:String):ParseError{
		
		var ref = Util.getLineCol(this.pos, input);
		
		var line = ref[0];
		var col = ref[0];
		this.failedChars.reverse();
		return new ParseError(this.pos, line, col, Util.uniq(this.nonterminals), this.failedChars);
	}
	
}