/**
* Waxeye Parser Generator
* www.waxeye.org
* Copyright (C) 2008 Orlando D. A. R. Hill
*
* Permission is hereby granted, free of charge, to any person obtaining a copy of
* this software and associated documentation files (the "Software"), to deal in
* the Software without restriction, including without limitation the rights to
* use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
* of the Software, and to permit persons to whom the Software is furnished to do
* so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
**/

/**
 * Javascript example for NodeJS.
 *
 * Install waxeye.js with:
 * mkdir -p ~/.node_libraries
 * ln -s /usr/local/waxeye/src/javascript/waxeye.js ~/.node_libraries/
 *
 * To compile the waxeye grammar, execute:
 *    ./bin/waxeye -g javascript src/example/javascript -p calc grammars/calc.waxeye
 *
 * Execute with:
 *    node nodejs_example.js
 */
var sys = require('sys');
var calc_parser = require('./calc_parser');

// Create our parser
var parser = new calc_parser.CalcParser();

// Parse our input
var ast = parser.parse("42");

// print our AST
sys.puts(ast);
