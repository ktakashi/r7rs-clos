R7RS CLOS
=========

Virgo is an R7RS CLOS library based on Tiny CLOS with syntax sugers.

Example
=======

```scheme
(import (scheme base)
	(scheme write)
	(virgo user))

(define-generic println)
(define-method println (s) (display 'top:) (write s) (newline))
(define-method println ((s <symbol>)) (display s) (newline))
(define-method println ((s <string>)) (call-next-method))

(define-class <foo> ()
  (slot 'init-form (lambda () 'ok))
  (slot2 'init-value 'ok2)
  )
(define-method print-object ((o <foo>) out write?)
  (display "#<foo " out)
  (display (slot-ref o 'slot) out)
  (display (slot-ref o 'slot2) out)
  (display ">"))
(define-method println ((f <foo>)) 
  (display (slot-ref f 'slot)) (display (slot-ref f 'slot2)) (newline))


(println 'symbol)
(println "string")
(println (make <foo>))
(println (make <foo> 'slot #t))
(display-object (make <foo> 'slot #t)) (newline)
```

APIs
====

TBD


