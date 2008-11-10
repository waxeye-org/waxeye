;;; Waxeye Parser Generator
;;; www.waxeye.org
;;; Copyright (C) 2008 Orlando D. A. R. Hill
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;; this software and associated documentation files (the "Software"), to deal in
;;; the Software without restriction, including without limitation the rights to
;;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is furnished to do
;;; so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

(template

 "foo=${bar};"
 (template (string #\f #\o #\o #\=) (code #\b #\a #\r) (string #\;))

 "public static void ${(get-method-name)}() {
    System.out.println(${(get-expr)});
}
"
 (template (string *) (code *) (string *) (code *) (string *))

 "def ${(get-name)}(${(get-args)}):${(i)}
${(get-code)}${(u)}"
 (template (string *) (code *) (string *) (code *) (string *) (code *) (string *) (code *) (code *))
)

(code

 "${}"
 (code)

 "${(foo 1)}"
 (code #\( #\f #\o #\o #\space #\1 #\))

 "${ a + b}"
 (code #\space #\a #\space #\+ #\space #\b)

 "${\\}}"
 (code #\})

 "${\\n}"
 (code #\\ #\n)

 "${$}"
 (code #\$)

 "${${}"
 (code #\$ #\{)

 "${}}"
 fail

 "${{}}"
 fail

 "${${}}"
 fail

 "${\\}"
 fail

 "${${\\}}"
 (code #\$ #\{ #\})
)

(string

 ";oisdcn;aosc;p981y2ep9nC"
 pass

 "$"
 pass

 "${"
 pass

 "$"
 pass

 "\\${}"
 pass

 "${\\}"
 pass

 ""
 fail
)
