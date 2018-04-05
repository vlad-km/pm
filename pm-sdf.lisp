;;;; -*- Mode: lisp; coding:utf-8 -*-

(lores:defsys :pm
    :path "git/pm"
    :components ((:file "package")
                 (:file "unify")
                 (:file "patmatch")
                 (:file "resume")))
