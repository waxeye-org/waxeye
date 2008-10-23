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


(Object

 ""
 fail

 "{}"
 (Object)

 "{ 1 }"
 fail

 "{ \"s\" }"
 fail

 "{ \"m\" : 1 }"
 (Object (Member (String #\m) (Value (Number #\1))))

 "{ \"\":{}, \"\":[]}"
 (Object (Member (String) (Value (Object))) (Member (String) (Value (Array))))
 )


(Array

 ""
 fail

 "[]"
 (Array)

 "[1,2]"
 (Array (Value (Number #\1)) (Value (Number #\2)))

 "[ 3 , [] , {} ]"
 (Array (Value (Number #\3)) (Value (Array)) (Value (Object)))
 )


(Number

 ""
 fail

 "0"
 (Number #\0)

 "1"
 (Number #\1)

 "01"
 fail

 "42"
 (Number #\4 #\2)

 "-4"
 (Number #\- #\4)

 "0.8"
 (Number #\0 #\. #\8)

 "7.635"
 (Number #\7 #\. #\6 #\3 #\5)

 "9e10"
 (Number #\9 #\e #\1 #\0)

 "4E+3"
 (Number #\4 #\E #\+ #\3)

 "-0e-517"
 (Number #\- #\0 #\e #\- #\5 #\1 #\7)
 )


(String

 ""
 fail

 "\"\""
 (String)

 "\"a\""
 (String #\a)

 "\"ab\""
 (String #\a #\b)

 "\"\\\"\""
 (String (Escaped #\"))

 "\"\\\\\""
 (String (Escaped #\\))
 )


(Escaped

 "u0000"
 (Escaped #\u #\0 #\0 #\0 #\0)

 "ua9F3"
 (Escaped #\u #\a #\9 #\F #\3)

 "u"
 fail

 "n"
 (Escaped #\n)

 "z"
 fail
 )
