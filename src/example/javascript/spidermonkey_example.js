/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

/**
 * Javascript example for spidermonkey.
 *
 * To compile the waxeye grammar, execute:
 *    ./bin/waxeye -g javascript src/example/javascript -p calc grammars/calc.waxeye
 *
 * Then copy waxeye.js to the src/example/javascript directory or modify the path to load.
 * Execute with:
 *    js spidermonkey_example.js
 */

load("waxeye.js");
load("parser.js");

// Create our parser
var p = new Parser();

// Parse our input
var ast = p.parse("42");

// Print our AST
print(ast);
