;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

(module
set
mzscheme

(provide within-set?)

;; Is 'b' within set 'a'?
(define (within-set? a b)
  (if (null? a)
      #f
      (let ((aa (car a)))
        (if (char? aa)
            (if (char=? aa b)
                #t
                (if (char<? aa b)
                    (within-set? (cdr a) b)
                    #f))
            (if (and (char<=? (car aa) b) (char<=? b (cdr aa)))
                #t
                (if (char<? (cdr aa) b)
                    (within-set? (cdr a) b)
                    #f))))))

)
