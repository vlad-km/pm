;;; -*- Mode: lisp; coding:utf-8 -*-
;;;
;;; Code from Paradigms of AI Programming
;;; Copyright (c) 1991 Peter Norvig
;;;
;;; Two bug fixes By Richard Fateman, rjf@cs.berkeley.edu  October 92.
;;;
;;; This code is ancient, like a mammoth shit.
;;; But, works, do not touch.
;;; Some small modifications (kludges) for the JSCL environment
;;; mvk, july, 2017
;;;      march, 2018
;;;
;;; This file is part of the PM package (pattern matching)
;;; Copyright © 2017,2018 Vladimir Mezentsev
;;;

;;; mvk
;;;   remove symbol-plist -> gethash
(defvar *segment-symbols* (make-hash-table :test #'eql))
(defvar *single-symbols* (make-hash-table :test #'eql))

(setf (gethash '?is *single-symbols*) 'match-is)
(setf (gethash '?or *single-symbols*) 'match-or)
(setf (gethash '?and *single-symbols*) 'match-and)
(setf (gethash '?not *single-symbols*) 'match-not)
(setf (gethash '?* *segment-symbols*) 'segment-match)
(setf (gethash '?+ *segment-symbols*) 'segment-match+)
(setf (gethash '?? *segment-symbols*) 'segment-match?)
(setf (gethash '?if *segment-symbols*)  'match-if)

;;; mvk
;;;    accessors for pm functional symbols
(defun segment-symbol-fn (first-pat-sym)
    (gethash first-pat-sym *segment-symbols*))

(defun single-symbol-fn (first-pat-sym)
    (gethash first-pat-sym *single-symbols*))


;;; mvk
;;;   rename pat-match -> match for :pm package
;;;   cond -> if then else
;;;   firts -> car
;;;   rest -> caar
;;;   remove duplication execution of the same code
;;;   from original text
(defun match (pattern input &optional (bindings +no-bindings+))
    (let ((mfn)
          (ps))
        (if (eql bindings +fail+)
            (return-from match +fail+)
            ;; else match variables
            (if (variable-p pattern)
                (return-from match (match-variable pattern input bindings))
                ;; else equal pattern input
                (if (equal pattern input)
                    (return-from match bindings)
                    ;; else segment pattern
                    (if (and (consp pattern)
                             (consp (car pattern))
                             (symbolp (setq ps (caar pattern)))
                             (setq mfn (segment-symbol-fn ps)))
                        (return-from match (funcall mfn pattern input bindings))
                        ;; else single pattern
                        (if (and (consp pattern)
                                 (symbolp (setq ps (car pattern)))
                                 (setq mfn (single-symbol-fn ps)))
                            (return-from match (funcall mfn (cdr pattern) input bindings))
                            ;; else match rests
                            (if (and (consp pattern) (consp input))
                                (match (cdr pattern) (cdr input) (match (car pattern) (car input) bindings))
                                ;; else fail
                                +fail+))))))))

;;; prog version
;;; note: slower than the functional style on the 0.001 sec
;;;
;;; mvk
;;; for debugging

#|
;;; debug
(defvar *ad* nil)
(defvar *always-true* t)


(defun allow-debug (&optional (allowed t)) (setq *ad* allowed))
(defun always-debug-true (&optional (always t)) (setq *always-true* always))


(defun debug (lvl  &rest args)
    (when *ad*
        (tabulate lvl)
        (dolist (it args) (princ it) (princ " "))
        (terpri))
    (if *always-true* *always-true* *always-true*))

(defun tabulate (lvl) (dotimes (i lvl) (princ " ")))


(defun match (pattern input &optional (bindings +no-bindings+))
    (prog ((mfn)
           (ps))

     empty-bindings
       (debug 0 'empty 'bindings bindings)
       (if (eql bindings +fail+)
           (return-from match +fail+))

     match-var
       ;; match variables
       (debug 0 'match-var pattern input )
       (unless (variable-p pattern) (go equals-both))
       (return-from match (match-variable pattern input bindings))

     equals-both
       ;; equal pattern input
       (debug 0 'equals-both pattern input)
       (unless (equal pattern input) (go match-segment))
       (return-from match  bindings)

     match-segment
       ;; segment pattern
       (debug 0 'match-segment :pat pattern :inp input :bind bindings)
       (unless (and (consp pattern)
                    (consp (car pattern))
                    (debug 3 :car-pattern (car pattern))
                    (debug 3 :caar-pattern (caar pattern))
                    (debug 3 :setq-ps (setq ps (caar pattern)))
                    (debug 3 :pred (symbolp ps))
                    (symbolp (setq ps (caar pattern)))
                    (debug 3 :ps ps)
                    (setq mfn (segment-symbol-fn ps)))
           (debug 2 'segment-unless :mfn mfn :ps ps :pattern pattern)
           (go match-single))
       (debug 1 'match-segment-fn :ps ps  :mfn mfn)
       (return-from match (funcall mfn pattern input bindings))

     match-single
       ;; else single pattern
       (debug 0 'match-single :pat pattern :inp input :bind bindings)
       (unless (and (consp pattern)
                    (symbolp (setq ps (car pattern)))
                    (debug 2 'single-fn ps 'fn (single-symbol-fn ps))
                    (setq mfn (single-symbol-fn ps)))
           (debug 2 'single-unless :mfn mfn :ps ps :pattern pattern)
           (go continue-match))
       (return-from match (funcall mfn (cdr pattern) input bindings))


     continue-match
       ;; else match rests
       (debug 0 'continue :pat pattern :inp input :bind bindings)
       (unless (and (consp pattern) (consp input)) (go exit))
       (return-from match (match (cdr pattern) (cdr input) (match (car pattern) (car input) bindings)))

     exit
       ;; else fail
       (debug 0 'exit :pat pattern :inp input :bind bindings)
       (return-from match +fail+)))
|#



;;; functionality

;;; ?is
;;;
;;; mvk
;;;   first -> car
;;;   second -> cadr
;;;   let* -> let
;;;   var&pred -> inline
(defun match-is (var-and-pred input bindings)
    "Succeed and bind var if the input satisfies pred,
  where var-and-pred is the list (var pred)."
    (let ((new-bindings (match (car var-and-pred) input bindings)))
        (if (or (eql new-bindings +fail+)
                (not (funcall (cadr var-and-pred) input)))
            +fail+
            new-bindings)))

;;; ?and
;;;
;;; mvk
;;;   cond -> if
;;;   first -> car
;;;   rest -> cdr
(defun match-and (patterns input bindings)
    "Succeed if all the patterns match the input."
    (if (eql bindings +fail+) +fail+
        (if (null patterns) bindings
            (match-and (cdr patterns) input
                       (match (car patterns) input
                           bindings)))))

;;; ?or
;;;
;;; mvk
;;;   first -> car
;;;   rest -> cdr
(defun match-or (patterns input bindings)
    "Succeed if any one of the patterns match the input."
    (if (null patterns)
        +fail+
        (let ((new-bindings (match (car patterns) input bindings)))
            (if (eql new-bindings +fail+)
                (match-or (cdr patterns) input bindings)
                new-bindings))))

;;; ?not
(defun match-not (patterns input bindings)
    "Succeed if none of the patterns match the input.
  This will never bind any variables."
    (if (match-or patterns input bindings) +fail+  bindings))

;;; ?*
;;; todo: subseq
(defun segment-match (pattern input bindings &optional (start 0))
    "Match the segment pattern ((?* var) . pat) against input."
    (let ((var (second (first pattern)))
          (pat (rest pattern)))
        (if (null pat)
            (match-variable var input bindings)
            (let ((pos (first-match-pos (first pat) input start)))
                (if (null pos)
                    +fail+
                    (let ((b2 (match
                                  pat
                                  (subseq input pos)
                                  (match-variable var (subseq input 0 pos) bindings))))
                        ;; If this match failed, try another longer one
                        (if (eq b2 +fail+)
                            (segment-match pattern input bindings (+ pos 1))
                            b2)))))))

;;; mvk
;;;   cond -> if
;;;   length -> list-length
;;; todo: position ?
(defun first-match-pos (pat1 input start)
    "Find the first position that pat1 could possibly match input,
  starting at position start.  If pat1 is non-constant, then just
  return start."
    (if (and (atom pat1) (not (variable-p pat1)))
        (position pat1 input :start start :test #'equal)
        (if (<= start (list-length input)) start)))

;;; ?+ (one or more)
;;; note: may be rename to ?orm ? (one or more)
;;;
(defun segment-match+ (pattern input bindings)
    "Match one or more elements of input."
    (segment-match pattern input bindings 1))

;;; ??
;;; note: may be rename to ?zoo
;;;       (a joke for those, who carefully read up to this place)
;;;
(defun segment-match? (pattern input bindings)
    "Match zero or one element of input."
    (let ((var (second (first pattern)))
          (pat (rest pattern)))
        (or (match (cons var pat) input bindings)
            (match pat input bindings))))

;;; ?if
;;; original fixed for jscl
;;; mvk
;;;
;;; note: bad performance
;;;       use times for 100 iterations with  patterns like:
;;;                  (?x ?op ?y is ?z (pm:?if (eql (funcall ?op ?x ?y) ?z)))
;;;                  (3 + 4 is 7)
;;;       over 6 secs (6.739)
;;; note: use pm:?if with carefull
;;;
(defun match-if (pattern input bindings)
    "Test an arbitrary expression involving variables.
  The pattern looks like ((?if code) . rest)."
    (let ((var) (val) (val1) (result))
        (and
         (eval
          `(let ,(dolist (it bindings result)
                     (setq var (car it) val (cdr it))
                     (setq val1 (if (symbolp val) `',val (escape-first-if val)))
                     (push (list var val1) result))
               ,(second (first pattern))))
         (match (rest pattern) input bindings))))

;;; mvk jscl kludge
(defun escape-first-if (val)
    (if (listp val) `',val val))

;;; note: not tested
;;;
;;; note: renamed for pm package
(defun match-abbrev (symbol expansion)
    "Define symbol as a macro standing for a pat-match pattern."
    (setf (symbol-plist symbol)
          (list 'expand-pat-match-abbrev (expand-match-abbrev expansion))))


;;; renamed for pm package
(defun expand-match-abbrev (pat)
    "Expand out all pattern matching abbreviations in pat."
    (cond ((and (symbolp pat) (get pat 'expand-pat-match-abbrev)))
          ((atom pat) pat)
          (t (cons (expand-match-abbrev (first pat))
                   (expand-match-abbrev (rest pat))))))

;;; EOF
