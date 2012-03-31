/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
var parser = require('./parser');

// Create our parser
var p = new parser.Parser();

// Parse our input
var ast = p.parse("42");

// Print our AST
console.log(ast.toString());
