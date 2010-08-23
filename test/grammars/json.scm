;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

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
