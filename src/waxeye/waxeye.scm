;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

(module
 waxeye
 mzscheme
 (require "main.scm")
 (main (vector->list (current-command-line-arguments))))
