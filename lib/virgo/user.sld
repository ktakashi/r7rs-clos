;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; virgo/user.sld - Modularised Tiny CLOS
;;;
;;;  Copyright (c) 2015  Takashi Kato  <ktakashi@ymail.com>
;;;  
;;;  Redistribution and use in source and binary forms, with or without
;;;  modification, are permitted provided that the following conditions
;;;  are met:
;;;  
;;;  1. Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;  
;;;  2. Redistributions in binary form must reproduce the above copyright
;;;     notice, this list of conditions and the following disclaimer in the
;;;     documentation and/or other materials provided with the distribution.
;;;  
;;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(define-library (virgo user)
  (export define-class
	  define-generic
	  define-method
	  make slot-ref slot-set!
	  print-object display-object write-object

	  <top> <object> <class>
	  <procedure-class> <entity-class> 
	  <generic> <method>
	  <boolean> <symbol> <char> <vector> <pair> <number>
	  <string> <procedure>)
  (import (scheme base)
	  (virgo core))
  (begin

    ;; you need sugar?
    (define-syntax define-generic
      (syntax-rules ()
	((_ name) (define name (make-generic 'name)))))

    ;; pain...
    (define-syntax extract
      (syntax-rules ()
	((_ symb body _cont)
	 (letrec-syntax
	     ((tr
	       (syntax-rules (symb)
		 ((_ x symb tail (cont-head symb-l . cont-args))
		  (cont-head (x . symb-l) . cont-args))
		 ((_ d (x . y) tail cont)
		  (tr x x (y . tail) cont))
		 ((_ d1 d2 () (cont-head  symb-l . cont-args))
		  (cont-head (symb . symb-l) . cont-args))
		 ((_ d1 d2 (x . y) cont)
		  (tr x x y cont)))))
	   (tr body body () _cont)))))


    (define-syntax define-method
      (syntax-rules ()
	((_ "cont" name specializer (args ...) (body ...))
	 (let-syntax
	     ((cont
	       (syntax-rules ()
		 ((cont (??call-next-method) (??args (... ...))
			(??body (... ...)))
		  (define dummy
		    (let ((m (make-method specializer
				  (lambda (??call-next-method ??args (... ...))
				    ??body (... ...)))))
		      (add-method name m)
		      m))))))
	   (extract call-next-method ((args ...) . (body ...))
		    (cont () (args ...) (body ...)))))
	
	((define-method "parse" name (specs ...) (vars ...)
	   ((var class) rest ...) body)
	 (define-method "parse" name (specs ... class) (vars ... var)
	   (rest ...) body))
	
	((define-method "parse" name (specs ...) (vars ...)
	   (var rest ...) body)
	 (define-method "parse" name (specs ... <top>) (vars ... var)
	   (rest ...) body))
	;; finish it
	((define-method "parse" name (specs ...) (vars ...) () body)
	 (define-method "cont" name (specs ...) (vars ...) body))
	((define-method name (spec ...) body ...)
	 (define-method "parse" name (list) ()  (spec ...) (body ...)))))


    (define-syntax define-class
      (syntax-rules ()
	((_ name (supers ...) (slot option ...) ...)
	 (define name
	   (make <class>
	     'direct-supers (list supers ... <object>)
	     'direct-slots (list (list 'slot option ...) ...))))))

    )
)
