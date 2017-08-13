package org.waxeye.parser;
import haxe.Json;

/**
 * ...
 * @author Damilare Akinlaja
 * Based on https://github.com/orlandohill/waxeye/blob/master/src/javascript/waxeye.coffee
 */
class ParseError
{
	public var pos:Int;
	public var line:Int;
	public var col:Int;
	public var nt:Array<Dynamic>;
	public var chars:Dynamic;

	public function new(pos:Int, line:Int, col:Int, nt:Array<Dynamic>, chars:Dynamic)
	{
		this.pos = pos;
		this.line = line;
		this.col = col;
		this.nt = nt;
		this.chars = chars;
		
	}

	public function toString():String
	{

		if (this.chars.charClasses != null)
		{
			this.chars = this.chars.charClasses;
		}

		if (this.chars != null)
		{
			this.chars = this.chars.map(function(ch:Dynamic)
			{
				//trace(ch);
				var str:String = '';
				if(ch.char != null){
					str = ch.char;
				}else if(ch.charClasses != null){
					str = ch.charClasses;
				}
				
				return Json.stringify(str);
			});
		}
		
		return "Parse Error: failed to match '" + this.nt.join(',') + "' at line=" + this.line+", col=" + this.col + ", pos=" + this.pos + " (expected '" + cast this.chars.map(
			function(s:String)
			{
				trace(s);
				return s.split("").slice(1, -1).join("");
			}
		).join(',') + "')";
	}

}