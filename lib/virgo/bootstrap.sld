;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; virgo/bootstrap.sld - Bootstap for Tiny CLOS
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

(define-library (virgo bootstrap)
  (export class-of slot-ref slot-set! class-name
	  class-direct-slots class-direct-supers class-slots class-cpl
	  generic-methods generic-name
	  method-specializers method-procedure
	  
	  make-class make-generic make-method

	  applicable? more-specific?

	  ;; generics
	  initialize allocate-instance compute-getter-and-setter
	  compute-slots compute-cpl compute-apply-generic compute-methods
	  compute-method-more-specific? compute-apply-methods
	  add-method
	  ;; classes
	  <top> <object> <class>
	  <procedure-class> <entity-class> 
	  <generic> <method>
	  <boolean> <symbol> <char> <vector> <pair> <number>
	  <string> <procedure>  <port> <unknown>
	  )
  ;; trick to load protocol first
  (import (scheme base) (scheme cxr) (virgo misc) (virgo protocol))
  (begin

;; Tiny CLOS copyright
; **********************************************************************
; Copyright (c) 1992 Xerox Corporation.  
; All Rights Reserved.  
;
; Use, reproduction, and preparation of derivative works are permitted.
; Any copy of this software or of any derivative work must include the
; above copyright notice of Xerox Corporation, this paragraph and the
; one after it.  Any distribution of this software or derivative works
; must comply with all applicable United States export control laws.
;
; This software is made available AS IS, and XEROX CORPORATION DISCLAIMS
; ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
; PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
; LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
; EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
; NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
; OF THE POSSIBILITY OF SUCH DAMAGES.
; **********************************************************************
;
(define gsort list-sort)

(define generic-invocation-generics (list compute-apply-generic
					  compute-methods
					  compute-method-more-specific?
					  compute-apply-methods))

(add-method compute-apply-generic
    (make-method (list <generic>)
	 (lambda (call-next-method generic)
	   (lambda args
	     (if (and (memq generic generic-invocation-generics)     ;* G  c
		      (memq (car args) generic-invocation-generics)) ;* r  a
		 (apply (method-procedure                            ;* o  s
			 (car (last-pair (generic-methods generic))));* u  e
			(cons #f args))                              ;* n
								     ;* d
		 ((compute-apply-methods generic)
		  ((compute-methods generic) args)
		  args))))))


(add-method compute-methods
    (make-method (list <generic>)
	 (lambda (call-next-method generic)
	   (lambda (args)
	     (let ((applicable
		    (filter-in (lambda (method)
			;
			; Note that every only goes as far as the
			; shortest list!
			;
				 (every applicable?
					(method-specializers method)
				        args))
			       (generic-methods generic))))
	       (gsort (lambda (m1 m2)
			((compute-method-more-specific? generic)
			 m1
			 m2
			 args))
			      applicable))))))


(add-method compute-method-more-specific?
    (make-method (list <generic>)
	 (lambda (call-next-method generic)
	   (lambda (m1 m2 args)
	     (let loop ((specls1 (method-specializers m1))
			(specls2 (method-specializers m2))
			(args args))
	       (cond ((null? specls1) #t)     ;*Maybe these two
		     ((null? specls2) #f)     ;*should barf?
		     ((null? args)
		      (error 'compute-method-more-specific?
			     "Fewer arguments than specializers."))
		     (else
		      (let ((c1  (car specls1))
			    (c2  (car specls2))
			    (arg (car args)))
			(if (eq? c1 c2)
			    (loop (cdr specls1)
				  (cdr specls2)
				  (cdr args))
			    (more-specific? c1 c2 arg))))))))))


(add-method compute-apply-methods
    (make-method (list <generic>)
	 (lambda (call-next-method generic)
	   (lambda (methods args)
	     (letrec ((one-step (lambda (tail)
				  (lambda ()
				    (apply (method-procedure (car tail))
					   (cons (one-step (cdr tail))
						 args))))))
	       ((one-step methods)))))))


(define unbound (list 'unbound))
(add-method initialize
    (make-method (list <object>)
	 (lambda (call-next-method object initargs) 
	   (let* ((class (class-of object))
		  (field-initializers (slot-ref class 'field-initializers))
		  (getters-n-setters (slot-ref class 'getters-n-setters)))
	     (for-each (lambda (fi gs)
			 (let* ((name (car gs))
				(v (getl initargs name unbound)))
			   (if (eq? v unbound)
			       ;; use field-initializers
			       ((caddr gs) object (fi))
			       ((caddr gs) object v))))
		       field-initializers getters-n-setters)
	     object))))

(add-method initialize
    (make-method (list <class>)
	 (lambda (call-next-method class initargs)
	   (call-next-method)
	   (slot-set! class
		      'direct-supers
		      (getl initargs 'direct-supers '()))
	   (slot-set! class
		      'direct-slots
		      (map (lambda (s)
			     (if (pair? s) s (list s)))
			   (getl initargs 'direct-slots  '())))
	   (slot-set! class 'cpl   (compute-cpl   class))
	   (slot-set! class 'slots (compute-slots class))
	   (let* ((nfields 0)
		  (field-initializers '())
		  (allocator
		   (lambda (init)
		     (let ((f nfields))
		       (set! nfields (+ nfields 1))
		       (set! field-initializers
			     (cons init field-initializers))
		       (list (lambda (o)   (get-field  o f))
			     (lambda (o n) (set-field! o f n))))))
		  (getters-n-setters
		   (map (lambda (slot)
			  (cons (car slot)
				(compute-getter-and-setter class
							   slot
							   allocator)))
			(slot-ref class 'slots))))
	     (slot-set! class 'nfields nfields)
	     (slot-set! class 'field-initializers 
			(reverse field-initializers))
	     (slot-set! class 'getters-n-setters getters-n-setters)))))

(add-method initialize
    (make-method (list <generic>)
	 (lambda (call-next-method generic initargs)
	   (call-next-method)
	   (slot-set! generic 'methods '())
	   (%set-entity-proc! generic
			      (lambda args (error 'initialize
						  "Has no methods."))))))

(add-method initialize
    (make-method (list <method>)
	 (lambda (call-next-method method initargs)
	   (call-next-method)
	   (slot-set! method 'specializers (getl initargs 'specializers))
	   (slot-set! method 'procedure    (getl initargs 'procedure)))))



(add-method allocate-instance
    (make-method (list <class>)
	 (lambda (call-next-method class)
	   (let* ((field-initializers (slot-ref class 'field-initializers))
		  (new (%allocate-instance
			class
		        (length field-initializers))))
	     (let loop ((n 0)
			(inits field-initializers))
	       (if (pair? inits)
		   (begin
		     (%instance-set! new n ((car inits)))
		     (loop (+ n 1)
			   (cdr inits)))
		   new))))))

(add-method allocate-instance
    (make-method (list <entity-class>)
	 (lambda (call-next-method class)
	   (let* ((field-initializers (slot-ref class 'field-initializers))
		  (new (%allocate-entity
			class
		        (length field-initializers))))
	     (let loop ((n 0)
			(inits field-initializers))
	       (if (pair? inits)
		   (begin
		     (%entity-set! new n ((car inits)))
		     (loop (+ n 1)
			   (cdr inits)))
		   new))))))


(add-method compute-cpl
    (make-method (list <class>)
	 (lambda (call-next-method class)
	   (compute-std-cpl class class-direct-supers))))


(add-method compute-slots
    (make-method (list <class>)
	 (lambda (call-next-method class)
	   (let collect ((to-process (apply append
					    (map class-direct-slots
						 (class-cpl class))))
			 (result '()))
	     (if (null? to-process)
		 (reverse result)
		 (let* ((current (car to-process))
		        (name (car current))
			(others '())
			(remaining-to-process
			 (collect-if (lambda (o)
				       (if (eq? (car o) name)
					   (begin
					     (set! others (cons o others))
					     #f)
					   #t))
				     (cdr to-process))))
		   (collect remaining-to-process
			    (cons (append current
					  (apply append (map cdr others)))
				  result))))))))

(add-method compute-getter-and-setter
    (make-method (list <class>)
	 (lambda (call-next-method class slot allocator)
	   (define (get slot name)
	     (cond ((memq name slot) => cadr)
		   (else #f)))
	   (let ((initialiser (or (get slot 'init-form)
				  (let ((v (get slot 'init-value)))
				    (and v (lambda () v)))
				  (lambda () (if #f #t)))))
	     (allocator initialiser)))))


    )
  )
