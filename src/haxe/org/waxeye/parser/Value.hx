package org.waxeye;
import org.waxeye.Util.Assert;
/**
 * ...
 * @author Damilare Akinlaja
 * Based on https://github.com/orlandohill/waxeye/blob/master/src/javascript/waxeye.coffee
 */
class Value
{
	public var type:String;
	public var err:Any;
	public var pos:Int;
	public var asts:Array<AST>;

	public function new(type:String, ?err:Any, ?pos:Int, ?asts:Array<AST>)
	{
		Assert.ok(Std.is(asts, Array));
		Assert.ok(Std.is(pos, Int));
		Assert.ok(Std.is(err, Class));

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