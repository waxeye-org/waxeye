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
load("calc_parser.js");

// Create our parser
var parser = new CalcParser();

// Parse our input
ast = parser.parse("42");

// print our AST
print(ast);
