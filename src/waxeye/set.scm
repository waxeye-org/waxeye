;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

(module
set
mzscheme

(provide subset?)

;; Is 'b' a subset of 'a'?
(define (subset? a b)
  (if (null? b)
      #t
      (if (null? a)
          #f
          (let ((aa (car a)) (bb (car b)))
            (if (char? aa)
                (if (and (char? bb) (char=? aa bb))
                    (subset? (cdr a) (cdr b))
                    #f)
                (if (char? bb)
                    (if (and (char<=? (car aa) bb) (char<=? bb (cdr aa)))
                        (subset? a (cdr b))
                        (subset? (cdr a) b))
                    (if (and (char<=? (car aa) (car bb)) (char<=? (cdr bb) (cdr aa)))
                        (subset? a (cdr b))
                        (subset? (cdr a) b))))))))

)
