package org.waxeye.parser;
import haxe.DynamicAccess;

/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Copyright (c) 2015 Joshua Gross
 * @author Damilare Akinlaja, 2017
 * Licensed under the MIT license. See 'LICENSE' for details.
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