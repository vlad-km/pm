# Pattern matching from PAIP

JSCL implementation of simple pattern matching from *Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp* by Peter Norvig (1992)

Original code see: https://github.com/norvig/paip-lisp/tree/master/lisp

Initially, the author's codes are written very inefficiently and need further development. But, works - do not touch it.

In the source code (unify.lisp, pat-match.lisp), I made some changes (kludges) for JSCL and Moren environment. 

`pat-match`, `pat-match-abbrev`, `expand-pat-match-abbrev` from original code were renamed to `pm:match`, `pm:match-abbrev`, `pm:expand-match-abbrev`

Note that the unification code for the matching is not the same as the unification code for horn logic.


## Use

```lisp

    (pm:match '(I need a ?x) '(I need a vacation)) 
    ;; => ((?X . VACATION))
    (pm:match '(I need a ?x) '(I really need a vacation))
    ;; => nil
    (pm:match '(this is easy) '(this is easy))
    ;;  => ((t . t))
    (pm:match '(?x is ?x) '((2 + 2) is 4)) 
    ;; => nil
    (pat-match '(?x is ?x) '((2 + 2) is (2 + 2)))
    ;; =>((?x 2 + 2))
    (pm:match '((?* ?p) need (?* ?x))
        '(Mr Hulot and I need a vacation))
    ;; => ((?X A VACATION) (?P MR HULOT AND I))
    (fset 'is-number (lambda (x) (numberp x)))
    (pm:match '(x = (?is ?n is-number)) '(x = 34))
    ;;  => ((?n . 34))
    (pm:match '(x = (?is ?n is-number)) '(x = x))
    ;;  => NIL
    (pm:match '(?x (?or < = >) ?y) '(3 < 4)) 
    ;; => ((?Y . 4) (?X . 3)))
    (pm:match '(x = (?and (?is ?n is-number) (?is ?n oddp))) '(x = 3)) 
    ;; => ((?N . 3))
    (pm:match '(?x /= (?not ?x)) '(3 /= 4)) 
    ;; => ((?X . 3))
    (pm:match '(?x > ?y (?if (> ?x ?y))) '(4 > 3))
    ;;  => ((?Y . 3) (?X . 4))
    (pm:match '(a (?* ?x) d) '(a b c d))
    ;;  => ((?X B C))
    (pm:match '(a (?* ?x) (?* ?y) d) '(a b c d))
    ;;  => ((?Y B C) (?X))
    (pm:match '(a (?* ?x) (?* ?y) ?x ?y) '(a b c d (b c) (d))) 
    ;;  => ((?Y D) (?X B C))
    (pm:match '(?x ?op ?y is ?z (?if (eql (funcall ?op ?x ?y) ?z))) '(3 + 4 is 7))
    ;;  => ((?Z . 7) (?Y . 4) (?OP . +) (?X . 3))
    (pm:match '(?x ?op ?y (?if (funcall ?op ?x ?y))) '(3 > 4))
    ;;  => NIL
    (let ((axyd))
        (pm:match-abbrev '?x* '(?* ?x)) 
        ;; => (?* ?X) 
        (pm:match-abbrev '?y* '(?* ?y))
        ;;  => (?* ?Y)
        (setf axyd (pm:expand-match-abbrev '(a ?x* ?y* d)))
        (pm:match axyd '(a b c d))
        ;;  => ((?Y B C) (?X))
        )
    (pm:match '(((?* ?x) (?* ?y)) ?x ?y) '((a b c d) (a b) (c d))) 
    ;;   => NIL
```

## Copyright
Copyright © 2017,2018 Vladimir Mezentsev

## License
GNU General Public License v3.0

