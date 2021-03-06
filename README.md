# Pattern matching

JSCL implementation of simple pattern matching from *Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp* by Peter Norvig (1992). Original code see: https://github.com/norvig/paip-lisp/tree/master/lisp

Initially, the author's codes are written very inefficiently and need further development. But, works - do not touch it.

In the source code, I made some changes (kludges) for JSCL and Moren environment. Now code optimization for MOREN environment (electron version).

`pat-match` from original code were renamed to `pm:match`.


## Status

`production`


# API

### Function

*pm:match* 


### Patterns

*pm:?**  *pm:?+  pm:??  pm::?if  pm:?is  pm:?or  pm:?and  pm:?not*


## Use

```lisp
    (require "./sys/pm")
    ;; or
    (lores:qload :pm)
```

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
    (pm:match '((pm:?* ?p) need (pm:?* ?x))'(Mr Hulot and I need a vacation))
    ;; => ((?X A VACATION) (?P MR HULOT AND I))
    (fset 'is-number (lambda (x) (numberp x)))
    (pm:match '(x = (pm:?is ?n is-number)) '(x = 34))
    ;;  => ((?n . 34))
    (pm:match '(x = (pm:?is ?n is-number)) '(x = x))
    ;;  => NIL
    (pm:match '(?x (pm:?or < = >) ?y) '(3 < 4)) 
    ;; => ((?Y . 4) (?X . 3)))
    (pm:match '(x = (pm:?and (pm:?is ?n is-number) (pm:?is ?n oddp))) '(x = 3)) 
    ;; => ((?N . 3))
    (pm:match '(?x /= (pm:?not ?x)) '(3 /= 4)) 
    ;; => ((?X . 3))
    (pm:match '(?x > ?y (pm:?if (> ?x ?y))) '(4 > 3))
    ;;  => ((?Y . 3) (?X . 4))
    (pm:match '(a (pm:?* ?x) d) '(a b c d))
    ;;  => ((?X B C))
    (pm:match '(a (pm:?* ?x) (pm:?* ?y) d) '(a b c d))
    ;;  => ((?Y B C) (?X))
    (pm:match '(a (pm:?* ?x) (pm:?* ?y) ?x ?y) '(a b c d (b c) (d))) 
    ;;  => ((?Y D) (?X B C))
    (pm:match '(?x ?op ?y is ?z (pm:?if (eql (funcall ?op ?x ?y) ?z))) '(3 + 4 is 7))
    ;;  => ((?Z . 7) (?Y . 4) (?OP . +) (?X . 3))
    (pm:match '(?x ?op ?y (pm:?if (funcall ?op ?x ?y))) '(3 > 4))
    ;;  => NIL
    (pm:match '(((pm:?* ?x) (pm:?* ?y)) ?x ?y) '((a b c d) (a b) (c d))) 
    ;;   => NIL
```

## Examples

```lisp
(defmacro times (iter &body body)
    `(time 
      (progn (dotimes (idx ,iter)
                 ,@body (values)) (values))))
```

```lisp
(progn
    (times 100 (pm:match '(I need a ?x) '(I need a vacation))) 
    (times 100 (pm:match '(I need a ?x) '(I really need a vacation)))
    (times 100 (pm:match '(this is easy) '(this is easy)))
    (times 100 (pm:match '(?x is ?x) '((2 + 2) is 4))) 
    (times 100 (pm:match '(?x is ?x) '((2 + 2) is (2 + 2))))
    (times 100 (pm:match '((pm:?* ?p) need (pm:?* ?x))'(Mr Hulot and I need a vacation)))
    (fset 'is-number (lambda (x) (numberp x)))
    (times 100 (pm:match '(x = (pm:?is ?n is-number)) '(x = 34)))
    (times 100 (pm:match '(x = (pm:?is ?n is-number)) '(x = x)))
    (times 100 (pm:match '(?x (pm:?or < = >) ?y) '(3 < 4))) 
    (times 100 (pm:match '(x = (pm:?and (pm:?is ?n is-number) (pm:?is ?n oddp))) '(x = 3))) 
    (times 100 (pm:match '(?x /= (pm:?not ?x)) '(3 /= 4))) 
    (times 100 (pm:match '(?x > ?y (pm:?if (> ?x ?y))) '(4 > 3)))
    (times 100 (pm:match '(a (pm:?* ?x) d) '(a b c d)))
    (times 100 (pm:match '(a (pm:?* ?x) (pm:?* ?y) d) '(a b c d)))
    (times 100 (pm:match '(a (pm:?* ?x) (pm:?* ?y) ?x ?y) '(a b c d (b c) (d)))) 
    (times 100 (pm:match '(?x ?op ?y is ?z (pm:?if (eql (funcall ?op ?x ?y) ?z))) '(3 + 4 is 7)))
    )
    ;;; =>
    Execution took 0.023 seconds.
    Execution took 0.009 seconds.
    Execution took 0.001 seconds.
    Execution took 0.018 seconds.
    Execution took 0.022 seconds.
    Execution took 0.033 seconds.
    Execution took 0.021 seconds.
    Execution took 0.018 seconds.
    Execution took 0.021 seconds.
    Execution took 0.03 seconds.
    Execution took 0.022 seconds.
    Execution took 7.074 seconds.
    Execution took 0.028 seconds.
    Execution took 0.034 seconds.
    Execution took 0.24 seconds.
    Execution took 6.799 seconds.
```

```lisp

   (pm:match '((pm:?* ?tail) (pm:?if (print ?tail))) '(start aaa bbb ccc ddd end))
   ;; =>
   NIL
   (START)
   (START AAA)
   (START AAA BBB)
   (START AAA BBB CCC)
   (START AAA BBB CCC DDD)
   (START AAA BBB CCC DDD END)
   ((?TAIL START AAA BBB CCC DDD END))

   (pm:match '(start (pm:?* ?tail) (pm:?if (print ?tail))) '(start aaa bbb ccc ddd end))
   ;;=>
   NIL
   (AAA)
   (AAA BBB)
   (AAA BBB CCC)
   (AAA BBB CCC DDD)
   (AAA BBB CCC DDD END)
   ((?TAIL AAA BBB CCC DDD END))


   (pm:match '(?a ?b (pm:?or (pm:?or 1 2 3) (pm:?is ?tail listp))) '(1 (2) (c)))
   ;;=>
   ((?TAIL C) (?B 2) (?A . 1))

   (pm:match '(?a ?b (pm:?or (pm:?or 1 2 3) (pm:?is ?tail listp))) '(1 (2) 3))
   ;;=>
   ((?B 2) (?A . 1))


```




## Copyright
Copyright � 2017,2018 Vladimir Mezentsev

## License
GNU General Public License v3.0

