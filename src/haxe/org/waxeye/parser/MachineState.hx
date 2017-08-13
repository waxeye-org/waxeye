package org.waxeye.parser;
import haxe.DynamicAccess;

/**
 * ...
 * @author Damilare Akinlaja
 */
class MachineState 
{
	public var type:String;
	public var result:Dynamic;
	public var configuration:MachineConfiguration;

	public function new(type:String = "", result:Dynamic, configuration:MachineConfiguration) 
	{
		
		this.type = type;
		this.result = result;
		this.configuration = configuration;
	}
	
	public static function FINAL(result:Dynamic):MachineState
	{
		return new MachineState("FINAL", result, null);
	}
	
	public static function INTER(configuration:MachineConfiguration):MachineState
	{
		return new MachineState("INTER", null, configuration);
	}
	
}