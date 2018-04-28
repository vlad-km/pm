;;; -*- Mode:lisp; coding:utf-8; -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig
;;;
;;; This code is ancient, like a mammoth shit.
;;; But, works, do not touch.
;;;
;;; Some small modifications for the JSCL environment
;;; in part of eq/eql/equal first/second
;;;         july, 2017, March, 2018. MVK
;;;
;;;
;;; This file is part of the PM package (pattern match package)
;;; Copyright © 2017,2018 Vladimir Mezentsev
;;;


(defconstant fail nil)
(defconstant no-bindings '((t . t)))

(defconstant +fail+ nil)
(defconstant *no-bindings* no-bindings)
(defconstant +no-bindings+ no-bindings)


(defun variable-p (x)
    "Is x a variable (a symbol beginning with `?')?"
    ;;(print (list 'variable-p x))
    (and (symbolp x) (eql (char (symbol-name x) 0) #\?)))


(defun match-variable (var input bindings)
    "Does VAR match input?  Uses (or updates) and returns bindings."
    (let ((binding (get-binding var bindings)))
        (cond ((not binding) (extend-bindings var input bindings))
              ((equal input (binding-val binding)) bindings)
              (t fail))))


(defun extend-bindings (var val bindings)
    "Add a (var . value) pair to a binding list."
    (cons (cons var val)
          ;; Once we add a "real" binding,
          ;; we can get rid of the dummy no-bindings
          ;; note: equal
          (if (equal bindings no-bindings)
              nil
              bindings)))


(defun binding-var (binding)
    "Get the variable part of a single binding."
    (car binding))

(defun binding-val (binding)
    "Get the value part of a single binding."
    (cdr binding))

(defun get-binding (var bindings)
    "Find a (variable . value) pair in a binding list."
    (assoc var bindings))

;;; EOF
