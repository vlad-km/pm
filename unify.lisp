;;; -*- Mode:lisp; coding:utf-8; -*-


;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;; Note that the unification code for the matching is not the same
;;; as the unification code for horn logic.
;;; This code is ancient, like a mammoth shit.
;;; But, works, do not touch.
;;; Some small modifications for the JSCL environment
;;; in part of eq/eql/equal
;;; july, 2017, March, 2018. MVK



(defconstant fail nil)
(defconstant no-bindings '((t . t)))

(defconstant +fail+ nil)
(defconstant *no-bindings* '((t . t)))
(defconstant +no-bindings+ no-bindings)
(defconstant +variable-prefix-char+ #\?)


(defun variable-p (x)
    "Is x a variable (a symbol beginning with `?')?"
    (and (symbolp x) (eql (char (symbol-name x) 0) #\?)))


(defun match-variable (var input bindings)
    "Does VAR match input?  Uses (or updates) and returns bindings."
    (let ((binding (get-binding var bindings)))
        (cond ((not binding) (extend-bindings var input bindings))
              ((equal input (binding-val binding)) bindings)
              (t fail))))


(defun make-binding (var val) (cons var val))

(defun binding-var (binding)
    "Get the variable part of a single binding."
    (car binding))

(defun binding-val (binding)
    "Get the value part of a single binding."
    (cdr binding))

(defun get-binding (var bindings)
    "Find a (variable . value) pair in a binding list."
    (assoc var bindings))

(defun lookup (var bindings)
    "Get the value part (for var) from a binding list."
    (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
    "Add a (var . value) pair to a binding list."
    (cons (cons var val)
          ;; Once we add a "real" binding,
          ;; we can get rid of the dummy no-bindings
          ;; note: equal !!!
          (if (equal bindings no-bindings)
              nil
              bindings)))

(defun reuse-cons (x y x-y)
    "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
    (if (and (eql x (car x-y)) (eql y (cdr x-y)))
        x-y
        (cons x y)))

;;; note:
(defparameter *occurs-check* t "Should we do the occurs check?")

(defun unify (x y &optional (bindings no-bindings))
    "See if x and y match with given bindings."
    (cond ((eql bindings fail) fail)
          ((equal x y) bindings)
          ((variable-p x) (unify-variable x y bindings))
          ((variable-p y) (unify-variable y x bindings))
          ((and (consp x) (consp y))
           (unify (rest x) (rest y)
                  (unify (first x) (first y) bindings)))
          (t fail)))

(defun unify-variable (var x bindings)
    "Unify var with x, using (and maybe extending) bindings."
    (cond ((get-binding var bindings)
           (unify (lookup var bindings) x bindings))
          ((and (variable-p x) (get-binding x bindings))
           (unify var (lookup x bindings) bindings))
          ((and *occurs-check* (occurs-check var x bindings))
           fail)
          (t (extend-bindings var x bindings))))

(defun occurs-check (var x bindings)
    "Does var occur anywhere inside x?"
    (cond ((equal var x) t)
          ((and (variable-p x) (get-binding x bindings))
           (occurs-check var (lookup x bindings) bindings))
          ((consp x) (or (occurs-check var (first x) bindings)
                         (occurs-check var (rest x) bindings)))
          (t nil)))

(defun subst-bindings (bindings x)
    "Substitute the value of variables in bindings into x,
  taking recursively bound variables into account."
    (cond ((eql bindings fail) fail)
          ((equal bindings no-bindings) x)
          ((and (variable-p x) (get-binding x bindings))
           (subst-bindings bindings (lookup x bindings)))
          ((atom x) x)
          (t (reuse-cons (subst-bindings bindings (car x))
                         (subst-bindings bindings (cdr x))
                         x))))

(defun unifier (x y)
    "Return something that unifies with both x and y (or fail)."
    (subst-bindings (unify x y) x))


;;; EOF
