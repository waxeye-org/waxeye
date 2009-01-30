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

var waxeye = (function() {

   /* ensure we have a false boolean and not empty array */
   var isFalse = function(value) {
     return typeof(value) == "boolean" && value == false;
   };

   var ord = function(string) {
     return (string+'').charCodeAt(0);
   };

   var extend = function(a, b) {
     if (a && b)  for (var i in b) a[i] = b[i];
     return a;
   };

   var iter = function() {
     var result = null;
     if (arguments.length == 2) { /* yield each array element */
       var ary = arguments[0], fun = arguments[1];
       for (var i = 0; i < ary.length; i++) { result = fun(ary[i]); }
     } else if (arguments.length == 3) { /* iter an exclusive range */
       var from = arguments[0], to = arguments[1], fun = arguments[2];
       for (var i = from; i < to; i++) { result = fun(i); }
     } else {
       throw "Invalid iterator arguments.";
     }
     return result;
   };


   var Edge = function(trans, state, voided) {
     return extend(this, {trans: trans, state: state, voided: voided});
   };


   var State = function(edges, match) {
     return extend(this, {edges: edges, match: match});
   };


   var FA = function(type, states, mode) {
     return extend(this, {type: type, states: states, mode: mode});
   };
   extend(FA, {VOID : 0, PRUNE: 1, LEFT: 2, POS: 3, NEG: 4});


   var ParseError = function(pos, line, col, nt) {
     return extend(this, { pos: pos, line: line, col: col, nt: nt });
   };
   extend(ParseError.prototype = {}, {
     toString: function() {
       var str = "parse error: failed to match '"+this.nt+"' ";
       str += "at line="+this.line+", col="+this.col+", pos="+this.pos+"\n";
       return str;
     }
   }); // ParseError


   var AST = function(type, children, pos) {
     return extend(this, {type: type, children: children, pos: pos});
   };
   extend(AST.prototype = {}, {

   strIter: function(ast, indent, acc) {
       var self = this;
       iter(0, indent[0] -1, function() {  acc.push('    '); });
       if (indent[0] > 0) { acc.push("->  "); }
       acc.push(ast.type);
       indent[0] += 1;
       iter(ast.children, function(a) {
         acc.push("\n");
         if(a instanceof AST) {
           self.strIter(a, indent, acc);
         } else {
           iter(0, indent[0] - 1, function() { acc.push("    "); });
           if(indent[0] > 0) { acc.push("|   "); }
           acc.push(a);
         }
       });
       indent[0] += 1;
     },

     toString: function() {
       var acc = [];
       this.strIter(this, [0], acc);
       return acc.join("");
     }
   }); // AST


   var WaxeyeParser = function(start, eof_check, automata) {
     return extend(this, {start: start, eof_check: eof_check, automata: automata});
   };
   extend(WaxeyeParser.prototype = {}, {
     parse: function(input) {
       return new InnerParser(this.start, this.eof_check, this.automata, input).parse();
     }
   }); // WaxeyeParser

   var InnerParser = function(start, eof_check, automata, input) {
     return extend(this, {
       start: start, eof_check: eof_check, automata: automata,
       input: input, input_len: input.length, input_pos: 0,
       line: 1, column: 0, last_cr: false,
       error_pos: 0, error_line: 1, error_col: 0,
       error_nt: automata[start].type, fa_stack: [], cache: {}
     });
   };
   extend(InnerParser.prototype = {}, {
     parse: function() {
       return this.do_eof_check(this.match_automaton(this.start));
     },

     match_automaton: function(index) {
       var start_pos = this.input_pos;
       var key = [index, start_pos];

       if (this.cache[key]) {
         var cachedItem = this.cache[key];
         this.restore_pos(cachedItem[1], cachedItem[2], cachedItem[3], cachedItem[4]);
         return cachedItem[0];
       }

       var start_line = this.line;
       var start_col = this.column;
       var start_cr = this.last_cr;
       var automaton = this.automata[index];
       var type = automaton.type;
       var mode = automaton.mode;

       this.fa_stack = [automaton].concat(this.fa_stack);
       var res = this.match_state(0);
       this.fa_stack = this.fa_stack.slice(1);

       var value;
       if(mode == FA.POS) {
         this.restore_pos(start_pos, start_line, start_col, start_cr);
         if (!isFalse(res)) {
           value = true;
         } else {
           value = false;
         }
       } else if (mode == FA.NEG) {
         this.restore_pos(start_pos, start_line, start_col, start_cr);
         if (!isFalse(res)) {
           value = this.update_error();
         } else {
           value = true;
         }
       } else if (!isFalse(res)) {
         if (mode == FA.VOID) {
           value = true;
         } else if (mode == FA.PRUNE) {
           var l = res.length;
           if (l == 0) {
             value = true;
           } else if (l == 1) {
             value = res[0];
           } else {
             value = new AST(type, res, [start_pos, this.input_pos]);
           }
         } else {
           value = new AST(type, res, [start_pos, this.input_pos]);
         }
       } else {
         value = this.update_error();
       }
       this.cache[key] = [value, this.input_pos, this.line, this.column, this.last_cr];
       return value;
     }, // match_automaton

     /** Returns a list of results so, need to check != false **/
     match_state: function(index) {
       var state = this.fa_stack[0].states[index];
       var res = this.match_edges(state.edges);
       if (!isFalse(res)) {
         return res;
       } else {
         return(state.match && []);
       }
     },

     match_edges: function(edges) {
       if (edges.length == 0) {
         return false;
       } else {
         var res = this.match_edge(edges[0]);
         if (!isFalse(res)) {
           return res;
         } else {
           return this.match_edges(edges.slice(1));
         }
       }
     },

     match_edge: function(edge) {
       var start_pos = this.input_pos;
       var start_line = this.line;
       var start_col = this.column;
       var start_cr = this.last_cr;
       var t = edge.trans;
       var res;

       if (t == -1) { // use -1 for wild card
         if (this.input_pos < this.input_len) {
           res = this.mv();
         } else {
           res = this.update_error();
         }
       } else if (typeof(t) == "string") {
         if (this.input_pos < this.input_len && t == this.input[this.input_pos]) {
           res = this.mv();
         } else {
           res = this.update_error();
         }
       } else if(t instanceof Array) {
         if (this.input_pos < this.input_len && this.within_set(t, ord(this.input[this.input_pos]))) {
           res = this.mv();
         } else {
           res = this.update_error();
         }
       } else if(typeof(t) == "number") {
         res = this.match_automaton(t);
       } else {
         res = false;
       }

       if (res) {
         var tran_res = this.match_state(edge.state);
         if (!isFalse(tran_res)) {
           if (edge.voided || res == true) {
             return tran_res;
           } else {
             return [res].concat(tran_res);
           }
         } else {
           this.restore_pos(start_pos, start_line, start_col, start_cr);
           return false;
         }
       } else {
         return false;
       }
     }, // match_edge

     restore_pos: function(pos, line, col, cr) {
       this.input_pos = pos;
       this.line = line;
       this.column = col;
       this.last_cr = cr;
     },

     update_error: function() {
       if (this.error_pos < this.input_pos) {
         this.error_pos = this.input_pos;
         this.error_line = this.line;
         this.error_col = this.column;
         this.error_nt = this.fa_stack[0].type;
       }
       return false;
     },

     mv: function() {
       var ch = this.input[this.input_pos];
       this.input_pos += 1;
       if (ch == "\r") {
         this.line += 1;
         this.column = 0;
         this.last_cr = true;
       } else {
         if (ch == "\n") {
           if(!this.last_cr) {
             this.line += 1;
             this.column = 0;
           }
         } else {
           this.column += 1;
         }
         this.last_cr = false;
       }
       return ch;
     },

     do_eof_check: function(res) {
       if(res) {
         if (this.eof_check && this.input_pos < this.input_len) {
           // Create a parse error - Not all input consumed
           return new ParseError(this.error_pos, this.error_line, this.error_col, this.error_nt);
         } else {
           return res;
         }
       } else {
         // Create a parse error
         return new ParseError(this.error_pos, this.error_line, this.error_col, this.error_nt);
       }
     },

     /** Takes a set and an ordinal **/
     within_set: function(set, c) {
       if (set.length == 0) {
         return false;
       } else {
         var aa = set[0];
         if (typeof(aa) == "string") {
           if (ord(aa) == c) {
             return true;
           } else {
             if (ord(aa) < c) {
               return this.within_set(set.slice(1), c);
             } else {
               return false;
             }
           }
         } else {
           // if not a string then must be a range (array)
           if (c > aa[0] && c <= aa[1]) {
             return true;
           } else {
             if (aa[1] < c) {
               return this.within_set(set.slice(1), c);
             } else {
               return false;
             }
           }
         }
       }
     } // within_set

   }); // InnerParser

   return {
     WaxeyeParser: WaxeyeParser,
     Edge: Edge, State: State, FA: FA
   };

})(); // waxeye
