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


(module
main
mzscheme

(require waxeye/ast
         scheme/cmdline
         "c.scm"
         "debug.scm"
         "file.scm"
         "gen.scm"
         "interp.scm"
         "java.scm"
         "javascript.scm"
         "load.scm"
         "python.scm"
         "ruby.scm"
         "scheme.scm"
         "tester.scm"
         "transform.scm"
         "util.scm"
         "version.scm")
(provide main)

(define *grammar-path* #f)
(define *grammar-test* #f)
(define *header-path* #f)
(define *interpret* #f)
(define *output-path* #f)
(define *target-lang* #f)


(define (main args)
  (process-args args)
  (when *grammar-path*
        (let ((grammar-tree (load-grammar *grammar-path*)))
          (transform-grammar grammar-tree)
          (start-nt! *start-name* grammar-tree)
          (cond
           (*interpret* (interpreter grammar-tree (input-as-string (current-input-port))))
           (*grammar-test* (tester grammar-tree *grammar-test*))
           ((and *target-lang* *output-path*)
            (begin
              (when *header-path*
                    (file-header! (file-as-string-lines *header-path*)))
              (display-version)
              (for-each (lambda (a)
                          (display-ln "generated: " a))
                        (*target-lang* grammar-tree *output-path*))))
           (else (display-help))))))


(define (process-args args)
  (if (member "--version" args)
      (display-version)
      (if (null? args)
          (begin
            (display-version)
            (newline)
            (display-help))
          (parse-args args))))


(define (parse-args args)
  (command-line
   #:program "waxeye"
   #:argv args

   #:help-labels "Waxeye modes:"

   #:once-any
   ("-g" language dir
    "Generate"
    (set! *target-lang* (case (string->symbol language)
                          ((c) gen-c)
                          ((java) gen-java)
                          ((python) gen-python)
                          ((ruby) gen-ruby)
                          ((scheme) gen-scheme)
                          ((javascript) gen-javascript)
                          (else #f)))
    (set! *output-path* (if (equal? (string-ref dir (- (string-length dir) 1)) #\/)
                            dir
                            (string-append dir "/"))))
   ("-i" "Interpret"
    (set! *interpret* #t))
   ("-t" test
    "Test"
    (set! *grammar-test* test))

   #:help-labels "Grammar options:"

   #:once-each
   ("-m"
    "Modular Grammar - default: false"
    (modular-grammar! #t))
   ("-s" start
    "Starting non-terminal - default: first non-terminal"
    (start-name! start))

   #:help-labels "Parser options:"

   #:once-each
   ("-c" comment
    "Header comment for generated files - default: none"
    (set! *header-path* comment))
   ("-e" eof
    "Check parser consumes all input - default: true"
    (eof-check! (equal? eof "true")))
   ("-n" namespace
    "Module or package namespace - default: none"
    (module-name! namespace))
   ("-p" prefix
    "Name prefix for generated files - default: none"
    (name-prefix! prefix))

   #:help-labels "Misc options:"

   #:once-each
   ("--debug" "Activates debug information"
    (debug! #t))
   ("--version" "Prints version number and copyright notice"
    (void))

   ;; expects one grammar path
   #:args (grammar)
   ;; set the grammar path when done
   (set! *grammar-path* grammar)))


(define (display-version)
  (display-ln "Waxeye Parser Generator v" *version*))


(define (display-help)
  (parse-args '("--help")))

)
