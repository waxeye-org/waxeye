;; A contrived example where we replace the definition of Number in Json with a
;; much simpler one that only supports integers.

(all-except "../json.waxeye" Number)

(rename (only "../num.waxeye" Num) (Num . Number))
