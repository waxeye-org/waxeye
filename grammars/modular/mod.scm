;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.


;; A contrived example where we replace the definition of Number in Json with a
;; much simpler one that only supports integers.

(all-except "../json.waxeye" Number)

(rename (only "../num.waxeye" Num) (Num . Number))
