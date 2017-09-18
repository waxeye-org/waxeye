package org.waxeye.parser;

/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Copyright (c) 2015 Joshua Gross
 * @author Damilare Akinlaja, 2017
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

@:enum
abstract Modes(String) 
{
	var NORMAL 	= 	"NORMAL";
	var PRUNING = 	"PRUNING";
	var VOIDING =	"VOIDING";
	
}