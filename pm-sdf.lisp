;;; -*- mode:lisp; coding:utf-8 -*-

;;; This file is part of the PM package (pattern matching)
;;; and intended for MOREN environment
;;; system definition file (LORES feature)
;;;
;;; Copyright © 2017,2018 Vladimir Mezentsev
;;;

(lores:defsys :pm
    :path "git/pm"
    :components ((:file "package")
                 (:file "binding")
                 (:file "match")
                 (:file "resume")))
