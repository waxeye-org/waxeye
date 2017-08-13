package org.waxeye.parser;
//import org.hamcrest.Matchers.*;
import org.waxeye.parser.RawError;
//using Lambda;

/**
 * ...
 * @author Damilare Akinlaja
 * Based on https://github.com/orlandohill/waxeye/blob/master/src/javascript/waxeye.coffee
 */
class Util
{

	public function new() {}

	public static function arrayPrepend(item:Any, ?a:Any):Any
	{
		
		if (a != null)
		{
			a = cast(a, Array<Dynamic>).slice(0);
		}

		cast(a, Array<Dynamic>).unshift(item);

		return a;
	}
	
	public static function uniq<T>(x:Array<T>):Array<T>
	{
			
			var r:Array<T> =  [];
			for (e in x){
			
				if (r.indexOf(e) == -1){
					r.push(e);
				}
			}
			return r;
			
	}
	
	
	public static function getLineCol(pos:Int, input:String):Array<Int>{
		
		var col = 0 , line = 0, lastLineBreak = 0;
		
		for (i in 0...pos){
			if (input.charAt(i) == '\r' && input.charAt(i + 1) == '\n'){
				continue;
			}
			
			var rn:Array<String> = ['\r', '\n'];
			
			if (rn.indexOf(input.charAt(i)) != -1){
				
				line++;
				lastLineBreak = i + 1;
					
			}
			col = i - lastLineBreak;
		}
		
		var ret:Array<Int> = [];
		ret.push(line+1);
		ret.push(col);
		
		return ret;
	}
	
	public static function first(a:Array<Dynamic>):Dynamic{
		if (a != null){
			return a[0];
		}
		
		return null;
	}
	
	public static function rest(a:Array<Dynamic>):Dynamic{
		if (a != null){
			return a.slice(1);
		}
		
		return null;
	}
	
	
	public static function updateError(err:RawError, pos:Int, e:Any):RawError
	{
		if (err != null && pos > err.pos){
			
			return new RawError(pos, [err.currentNT], [e], err.currentNT);
			
		} else if (err == null || pos == err.pos){
			
			if(err.pos == 0){
				err.pos = 0;
			}
			
			if(err.currentNT == null){
				err.currentNT = "";
			}
			
			if(err.nonterminals == null){
				err.nonterminals = [];
			}
			
			if(err.failedChars == null){
				err.failedChars = [];
			}			
			
			return new RawError(err.pos, Util.arrayPrepend(err.currentNT, err.nonterminals), Util.arrayPrepend(e, err.failedChars), err.currentNT);
		} else{
			return new RawError(err.pos, err.nonterminals, err.failedChars, err.currentNT);
		} 
		
	}
	

}

class Assert
{

	public function new(val:Bool)
	{

		ok(val);
	}

	public static function ok(val:Bool)
	{
		if (val == false)
		{

			throw 'assertion error';
		}
	}
}



