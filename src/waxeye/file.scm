;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

(module
file
mzscheme

(provide (all-defined))

(define (input-as-string port)
  (define (input-as-iter)
    (let ((ch (read-char port)))
      (if (eof-object? ch)
          '()
          (cons ch (input-as-iter)))))
  (list->string (input-as-iter)))


;; Returns the contents of the file of the given name as a string
(define (file-as-string path)
  (call-with-input-file path input-as-string))


(define (file-as-string-lines path)
  (define (file-as-iter stream)
    (let ((ch (read-line stream)))
      (if (eof-object? ch)
          '()
          (cons ch (file-as-iter stream)))))
  (call-with-input-file path file-as-iter))

)
