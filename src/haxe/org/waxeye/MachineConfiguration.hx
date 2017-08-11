package org.waxeye;
import org.waxeye.Util.Assert;

/**
 * ...
 * @author Damilare Akinlaja
 *
 * Based on https://github.com/orlandohill/waxeye/blob/master/src/javascript/waxeye.coffee
 */
class MachineConfiguration
{
	public var type:String;
	public var exp:Exp;
	public var pos:Int;
	public var asts:Array<AST>;
	public var err:Any;
	public var continuations:Array<Continuations>;
	public var value:Value;

	public function new(type:String, exp:Exp, pos:Int, asts:Array<AST>, err:Any, continuations:Array<Continuations>, value:Value)
	{
		this.type = type;
		this.exp = exp;
		this.pos = pos;
		this.asts = asts;
		this.err = err;
		this.continuations = continuations;
		this.value = value;

		Assert.ok(Std.is(this.asts, Array));
		Assert.ok(Std.is(this.continuations, Array));
		Assert.ok(Std.is(pos, Int));
	}
	
	
	public static function EVAL(exp:Exp, pos:Int, asts:Array<AST>, err:Any, continuations:Array<Continuations>):MachineConfiguration
	{
		return new MachineConfiguration("EVAL", exp, pos, asts, err, continuations, null);
	}

	
	public static function APPLY(continuations:Array<Continuations>, value:Value):MachineConfiguration
	{
		return new MachineConfiguration("APPLY", null, 0, null, null, continuations, value);
	}	
}