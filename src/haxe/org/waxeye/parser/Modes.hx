package org.waxeye.parser;

/**
 * ...
 * @author Damilare Akinlaja
 * Based on https://github.com/orlandohill/waxeye/blob/master/src/javascript/waxeye.coffee
 */

@:enum
abstract Modes(String) 
{
	var NORMAL 	= 	"NORMAL";
	var PRUNING = 	"PRUNING";
	var VOIDING =	"VOIDING";
	
}