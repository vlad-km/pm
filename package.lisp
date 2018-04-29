;;; -*- mode:lisp; coding:utf-8 -*-

;;; This file is part of the PM package (pattern matching)
;;; Copyright © 2017,2018 Vladimir Mezentsev
;;;


(defpackage :pm
  (:use #:cl)
  (:export #:?is #:?or #:?and #:?not #:?* #:?+ #:?? #:?if
           #:match
           #:match-abbrev
           #:expand-match-abbrev))


(in-package :pm)


;;; EOF
