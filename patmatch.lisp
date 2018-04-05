;;;; -*- Mode: lisp; coding:utf-8 -*-



;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;; Two bug fixes By Richard Fateman, rjf@cs.berkeley.edu  October 92.

;;; This code is ancient, like a mammoth.
;;; But, works, do not touch.
;;;
;;; Some small modifications (kludges) for the JSCL environment
;;; mvk, july, 2017
;;;      march, 2018

(setf (symbol-plist '?is) (list 'single-match 'match-is))
(setf (symbol-plist '?or) (list 'single-match 'match-or))
(setf (symbol-plist '?and) (list 'single-match 'match-and))
(setf (symbol-plist '?not) (list 'single-match 'match-not))
(setf (symbol-plist '?*) (list 'segment-match 'segment-match))
(setf (symbol-plist '?+) (list 'segment-match 'segment-match+))
(setf (symbol-plist '??) (list 'segment-match 'segment-match?))
(setf (symbol-plist '?if) (list 'segment-match 'match-if))

(defun match (pattern input &optional (bindings +no-bindings+))
    "Match pattern against input in the context of the bindings"
    (cond ((eql bindings +fail+) +fail+)
          ((variable-p pattern) (match-variable pattern input bindings))
          ((equal pattern input) bindings)
          ((segment-pattern-p pattern) (segment-matcher pattern input bindings))
          ((single-pattern-p pattern) (single-matcher pattern input bindings))
          ((and (consp pattern) (consp input))
           (match
               (rest pattern)
               (rest input)
               (match (first pattern) (first input) bindings)))
          (t +fail+)))

;;; segment-pattern-p
(defun segment-pattern-p (pattern)
    "Is this a segment-matching pattern like ((?* var) . pat)?"
    (and (consp pattern) (consp (first pattern))
         (symbolp (caar pattern))
         (get (intern (symbol-name (caar pattern)) :pm) 'segment-match )))

(defun segment-matcher (pattern input bindings)
    "Call the right function for this kind of segment pattern."
    (funcall (get (intern (symbol-name (caar pattern)) :pm) 'segment-match)
             pattern input bindings))


;;; single-pattern-p
;;;

(defun single-pattern-p (pattern)
    "Is this a single-matching pattern?
  E.g. (?is x predicate) (?and . patterns) (?or . patterns)."
    (and (consp pattern)
         (symbolp (first pattern))
         (get (intern (symbol-name (first pattern)) :pm) 'single-match)))

(defun single-matcher (pattern input bindings)
    "Call the right function for this kind of single pattern."
    (funcall (get (intern (symbol-name (first pattern)) :pm) 'single-match )
             (rest pattern)
             input
             bindings))

;;; functionality
(defun match-is (var-and-pred input bindings)
    "Succeed and bind var if the input satisfies pred,
  where var-and-pred is the list (var pred)."
    (let* ((var (first var-and-pred))
           (pred (second var-and-pred))
           (new-bindings (match var input bindings)))
        (if (or (eql new-bindings +fail+)
                (not (funcall pred input)))
            +fail+
            new-bindings)))

(defun match-and (patterns input bindings)
    "Succeed if all the patterns match the input."
    (cond ((eql bindings +fail+) +fail+)
          ((null patterns) bindings)
          (t (match-and (rest patterns) input
                        (match (first patterns) input
                            bindings)))))

(defun match-or (patterns input bindings)
    "Succeed if any one of the patterns match the input."
    (if (null patterns)
        +fail+
        (let ((new-bindings (match (first patterns) input bindings)))
            (if (eql new-bindings +fail+)
                (match-or (rest patterns) input bindings)
                new-bindings))))

(defun match-not (patterns input bindings)
    "Succeed if none of the patterns match the input.
  This will never bind any variables."
    (if (match-or patterns input bindings)
        +fail+
        bindings))

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


(defun first-match-pos (pat1 input start)
    "Find the first position that pat1 could possibly match input,
  starting at position start.  If pat1 is non-constant, then just
  return start."
    (cond ((and (atom pat1) (not (variable-p pat1)))
           (position pat1 input :start start :test #'equal))
          ((<= start (length input)) start) ;*** fix, rjf 10/1/92 (was <)
          (t nil)))

(defun segment-match+ (pattern input bindings)
    "Match one or more elements of input."
    (segment-match pattern input bindings 1))

(defun segment-match? (pattern input bindings)
    "Match zero or one element of input."
    (let ((var (second (first pattern)))
          (pat (rest pattern)))
        (or (match (cons var pat) input bindings)
            (match pat input bindings))))

;;; fixed for jscl
;;; mvk
(defun match-if (pattern input bindings)
    "Test an arbitrary expression involving variables.
  The pattern looks like ((?if code) . rest)."
    (let ((vars (mapcar #'car bindings))
          (vals (mapcar #'cdr bindings))
          (fn (second (first pattern))))
        (and
         (eval
          `(let ,(mapcar
                  (lambda (var val)
                      (list var
                            (if (and (symbolp val) (fboundp val)) `#',val  val)))
                  vars vals)
               ,fn))
         (match (rest pattern) input bindings))
        ))


;;; renamed for pm package
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
