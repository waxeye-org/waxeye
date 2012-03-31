/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
var parser = require('./parser');

var input = "";
var stdin = process.openStdin();

stdin.setEncoding('utf8');

// Read our input
stdin.on('data', function (chunk) {
    input += chunk;
});

stdin.on('end', function () {
    // Create our parser
    var p = new parser.Parser();

    // Parse our input
    var ast = p.parse(input);

    // Print our AST
    console.log(ast.toString());
});
