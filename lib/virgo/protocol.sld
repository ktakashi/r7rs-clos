;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; virgo/protocol.sld - Protocol for Tiny CLOS
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

(define-library (virgo protocol)
  (export class-of slot-ref slot-set!
	  class-direct-slots class-direct-supers class-slots class-cpl
	  class-name

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
	  <string> <procedure> <port> <unknown>

	  ;; internal use only
	  %allocate-instance %instance-set! compute-std-cpl
	  %allocate-entity %entity-set! %entity? %entity-class
	  %set-entity-proc!
	  %instance? %instance-class
	  set-field! get-field
	  )
  (import (scheme base)
	  (virgo misc))
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

(define compute-std-cpl
  (lambda (c get-direct-supers)
    (top-sort ((build-transitive-closure get-direct-supers) c)
	      ((build-constraints get-direct-supers) c)
	      (std-tie-breaker get-direct-supers))))


(define top-sort
  (lambda (elements constraints tie-breaker)
    (let loop ((elements    elements)
	       (constraints constraints)
	       (result      '()))
      (if (null? elements)
	  result
	  (let ((can-go-in-now
		 (filter-in
		  (lambda (x)
		    (every (lambda (constraint)
			     (or (not (eq? (cadr constraint) x))
				 (memq (car constraint) result)))
			   constraints))
		  elements)))
	    (if (null? can-go-in-now)
		(error "top-sort: Invalid constraints")
		(let ((choice (if (null? (cdr can-go-in-now))
				  (car can-go-in-now)
				  (tie-breaker result
					       can-go-in-now))))
		  (loop
		   (filter-in (lambda (x) (not (eq? x choice)))
			      elements)
		   constraints
		   (append result (list choice))))))))))

(define std-tie-breaker
  (lambda (get-supers)
    (lambda (partial-cpl min-elts)
      (let loop ((pcpl (reverse partial-cpl)))
	(let ((current-elt (car pcpl)))
	  (let ((ds-of-ce (get-supers current-elt)))
	    (let ((common (filter-in (lambda (x) (memq x ds-of-ce)) min-elts)))
	      (if (null? common)
		  (if (null? (cdr pcpl))
		      (error "std-tie-breaker Nothing valid")
		      (loop (cdr pcpl)))
		  (car common)))))))))


(define build-transitive-closure
  (lambda (get-follow-ons)
    (lambda (x)
      (let track ((result '())
		  (pending (list x)))
	(if (null? pending)
	    result
	    (let ((next (car pending)))
	      (if (memq next result)
		  (track result (cdr pending))
		  (track (cons next result)
			 (append (get-follow-ons next)
				 (cdr pending))))))))))

(define build-constraints
  (lambda (get-follow-ons)
    (lambda (x)
      (let loop ((elements ((build-transitive-closure get-follow-ons) x))
		 (this-one '())
		 (result '()))
	(if (or (null? this-one) (null? (cdr this-one)))
	    (if (null? elements)
		result
		(loop (cdr elements)
		      (cons (car elements)
			    (get-follow-ons (car elements)))
		      result))
	    (loop elements
		  (cdr this-one)
		  (cons (list (car this-one) (cadr this-one))
			result)))))))

;; use record
(define-record-type <instance> (make-instance class slots) instance?
  (class instance-class set-instance-class!)
  (slots instance-slots))

(define %allocate-instance
  (lambda (class nfields)
    (let ((slots (make-vector (+ nfields 2) #f)))
      (make-instance class slots))))
(define %instance? instance?)
(define %instance-class instance-class)
(define %set-instance-class! set-instance-class!)
(define %instance-ref
  (lambda (instance index)
    (vector-ref (instance-slots instance) index)))
(define %instance-set!
  (lambda (instance index new-value)
    (vector-set! (instance-slots instance) index new-value)))

(define entities '())

(define-record-type <entity> (make-entity proc class slots) entity?
  (proc entity-proc set-entity-proc!)
  (class entity-class)
  (slots entity-slots))

(define get-entity
  (lambda (closure)
    (let ((cell (assq closure entities)))
      (and cell (cdr cell)))))
(define default-proc
  (lambda args
    (error "default-proc: Called entity without first setting proc.")))
(define %allocate-entity
  (lambda (class nfields)
    (letrec ((entity (make-entity #f class (make-vector nfields #f)))
	     (closure (lambda args
			(apply (entity-proc entity) args))))
      (set-entity-proc! entity default-proc)
      (set! entities (cons (cons closure entity) entities))
      closure)))
(define %entity? (lambda (o) (entity? (get-entity o))))
(define %set-entity-proc! 
  (lambda (closure proc)
    (set-entity-proc! (get-entity closure) proc)))
(define %entity-class 
  (lambda (closure)
    (entity-class (get-entity closure))))
(define %entity-ref
  (lambda (closure index)
    (let ((vector (entity-slots (get-entity closure))))
      (vector-ref vector index))))
(define %entity-set!
  (lambda (closure index new-value)
    (let ((vector (entity-slots (get-entity closure))))
      (vector-set! vector index new-value))))

(define get-field
  (lambda (object field)
    (cond ((%instance? object) (%instance-ref object field))
	  ((%entity?   object) (%entity-ref   object field))
	  (else
	   (error 'get-field
		  "Can only get-field of instances and entities.")))))

(define set-field!
  (lambda (object field new-value)
    (cond ((%instance? object) (%instance-set! object field new-value))
	  ((%entity?   object) (%entity-set!   object field new-value))
	  (else
	   (error 'set-field!
		  "Can only set-field! of instances and entities.")))))

(define (%make-class class . initargs)
  ;; ugly but no choice
  (define slot-ref
    (lambda (object slot-name)
      (let* ((info   (lookup-slot-info (%instance-class object) slot-name))
	     (getter (car info)))
	(getter object))))

  (define slot-set!
    (lambda (object slot-name new-value)
      (let* ((info   (lookup-slot-info (%instance-class object) slot-name))
	     (setter (cadr info)))
	(setter object new-value))))

  (define lookup-slot-info
    (lambda (class slot-name)
      (let* ((getters-n-setters getters-n-setters-for-class)
	     (entry (assq slot-name getters-n-setters)))
	(if (null? entry)
	    (error (string-append 
		    "lookup-slot-info:"
		    "No slot" (symbol->string slot-name)
		    "in instances of") class)
	    (cdr entry)))))

  (let* ((new (%allocate-instance class
				  (length the-slots-of-a-class)))
	 (dsupers (getl initargs 'direct-supers '()))
	 (dslots  (map list
		       (getl initargs 'direct-slots  '())))
	 (cpl     (let loop ((sups dsupers) (so-far (list new)))
		    (if (null? sups)
			(reverse so-far)
			(loop (slot-ref (car sups) 'direct-supers)
			      (cons (car sups)
				    so-far)))))
	 (slots (apply append
		       (cons dslots
			     (map (lambda (c) (slot-ref c 'direct-slots))
				  (cdr cpl)))))
	 (nfields 0)
	 (field-initializers '())
	 (name (getl initargs 'name #f))
	 (allocator
	  (lambda (init)
	    (let ((f nfields))
	      (set! nfields (+ nfields 1))
	      (set! field-initializers
		    (cons init field-initializers))
	      (list (lambda (o)   (get-field  o f))
		    (lambda (o n) (set-field! o f n))))))
	 (getters-n-setters
	  (map (lambda (s)
		 (cons (car s)
		       (allocator (lambda () '()))))
	       slots)))
    (slot-set! new 'name name)
    (slot-set! new 'direct-supers      dsupers)
    (slot-set! new 'direct-slots       dslots)
    (slot-set! new 'cpl                cpl)
    (slot-set! new 'slots              slots)
    (slot-set! new 'nfields            nfields)
    (slot-set! new 'field-initializers (reverse
					field-initializers))
    (slot-set! new 'getters-n-setters  getters-n-setters)
    new))

(define slot-ref
  (lambda (object slot-name)
    (let* ((info   (lookup-slot-info (class-of object) slot-name))
	   (getter (car info)))
      (getter object))))

(define slot-set!
  (lambda (object slot-name new-value)
    (let* ((info   (lookup-slot-info (class-of object) slot-name))
	   (setter (cadr info)))
      (setter object new-value))))

(define lookup-slot-info
  (lambda (class slot-name)
    (let* ((getters-n-setters
	    (if (eq? class <class>)           ;* This grounds out
		getters-n-setters-for-class   ;* the slot-ref tower.
		(slot-ref class 'getters-n-setters)))
	   (entry (assq slot-name getters-n-setters)))
      (if (null? entry)
	  (error (string-append 
		  "lookup-slot-info:"
		  "No slot" (symbol->string slot-name)
		  "in instances of") class)
	  (cdr entry)))))

(define class-name
  (lambda (class) (slot-ref class 'name)))
(define class-direct-slots
  (lambda (class) (slot-ref class 'direct-slots)))
(define class-direct-supers
  (lambda (class) (slot-ref class 'direct-supers)))
(define class-slots
  (lambda (class) (slot-ref class 'slots)))
(define class-cpl
  (lambda (class) (slot-ref class 'cpl)))

(define generic-methods
  (lambda (generic) (slot-ref generic 'methods)))
(define generic-name
  (lambda (generic) (slot-ref generic 'name)))

(define method-specializers
  (lambda (method) (slot-ref method 'specializers)))
(define method-procedure
  (lambda (method) (slot-ref method 'procedure)))

(define the-slots-of-a-class     ;
  '(name		       ;name of the class
    direct-supers              ;(class ...)        
    direct-slots               ;((name . options) ...)
    cpl                        ;(class ...) 
    slots                      ;((name . options) ...) 
    nfields                    ;an integer
    field-initializers         ;(proc ...)
    getters-n-setters          ;((slot-name getter setter) ...)
    ))
					;
(define getters-n-setters-for-class      ;see lookup-slot-info
  (let ((make-em (lambda (s f)
		   (list s
			 (lambda (o)   (%instance-ref  o f))
			 (lambda (o n) (%instance-set! o f n))))))
    (map (lambda (s)
	   (make-em s (position-of s the-slots-of-a-class)))
	 the-slots-of-a-class)))
(define-values (<class> <top> <object>)
  (let ((c (%allocate-instance #f (length the-slots-of-a-class))))
    (%set-instance-class! c c)
    (let* ((t (%make-class c 'direct-supers (list) 'direct-slots  (list)))
	   (o (%make-class c 'direct-supers (list t) 'direct-slots  (list))))
      (%instance-set! c 0 '<class>)			   ;name
      (%instance-set! c 1 (list o))			   ;d supers
      (%instance-set! c 2 (map list the-slots-of-a-class))  ;d slots
      (%instance-set! c 3 (list c o t))			    ;cpl
      (%instance-set! c 4 (map list the-slots-of-a-class))  ;slots
      (%instance-set! c 5 (length the-slots-of-a-class))    ;nfields
      (%instance-set! c 6 (map (lambda (s)                  ;field-ini..
				 (lambda () '()))
			       the-slots-of-a-class))
      (%instance-set! c 7 '())                              ;not needed
      (values c t o))))

(define <procedure-class> (%make-class <class>
			    'name '<procedure-class> 
			    'direct-supers (list <class>)
			    'direct-slots  (list)))

(define <entity-class>    (%make-class <class>
			    'name '<entity-class> 
			    'direct-supers (list <procedure-class>)
			    'direct-slots  (list)))

(define <generic>         (%make-class <class>
			    'name '<generic> 
			    'direct-supers (list <object>)
			    'direct-slots  (list 'name 'methods)))

(define <method>          (%make-class <class>
			    'name '<method> 
			    'direct-supers (list <object>)
			    'direct-slots  (list 'specializers
						 'procedure)))

(define <primitive-class>
  (%make-class <class>
    'name '<primitive-class>
    'direct-supers (list <class>)
    'direct-slots  (list)))

(define make-primitive-class
  (lambda (name . class)
    (%make-class (if (null? class) <primitive-class> (car class))
      'name name
      'direct-supers (list <top>)
      'direct-slots  (list))))

(define-syntax define-primitive-class
  (syntax-rules ()
    ((_ name) (define name (make-primitive-class 'name)))
    ((_ name class) (define name (make-primitive-class 'name class)))))

(define-primitive-class <boolean>)
(define-primitive-class <symbol> )
(define-primitive-class <char>   )
(define-primitive-class <vector> )
(define-primitive-class <pair>   )
(define-primitive-class <number> )
(define-primitive-class <string> )
(define-primitive-class <port>   )
(define-primitive-class <unknown>)
(define-primitive-class <procedure> <procedure-class>)

(define class-of
  (lambda (x)
    (cond ((%instance? x)  (%instance-class x))
	  ((%entity? x)    (%entity-class x))

	  ((boolean? x)    <boolean>)
	  ((symbol? x)     <symbol>)
	  ((char? x)       <char>)
	  ((vector? x)     <vector>)
	  ((pair? x)       <pair>)
	  ((number? x)     <number>)
	  ((string? x)     <string>)
	  ((procedure? x)  <procedure>)
	  ((port? x)       <port>)
	  (else            <unknown>))))

(define make-class
  (lambda (direct-supers direct-slots)
    (%make-class <class>
      'direct-supers direct-supers
      'direct-slots  direct-slots)))

(define make-generic
  (lambda name 
    (let ((new (%allocate-entity <generic>
				 (length (class-slots <generic>)))))
      (slot-set! new 'methods '())
      (unless (null? name) (slot-set! new 'name (car name)))
      new)))

(define make-method
  (lambda (specializers procedure)
    (let ((new (%allocate-instance
	      <method>
	      (length (class-slots <method>)))))
      (slot-set! new
		 'specializers
		 specializers)
      (slot-set! new
		 'procedure
		 procedure)
      new)))

(define initialize (make-generic 'initialize))
(define allocate-instance (make-generic 'allocate-instance))
(define compute-getter-and-setter (make-generic 'compute-getter-and-setter))
(define compute-cpl (make-generic 'compute-cpl))
(define compute-slots (make-generic 'compute-slots))
(define compute-apply-generic
  (let ((g (make-generic 'compute-apply-generic)))
    (%set-entity-proc! g
       (lambda (generic)		;The ONE time this is called
					;it doesn't get cnm.
	 (lambda args
	   (apply (method-procedure (car (generic-methods generic)))
		  (cons #f args)))))
    g))

(define compute-methods               (make-generic 'compute-methods))
(define compute-method-more-specific? 
  (make-generic 'compute-method-more-specific?))
(define compute-apply-methods         (make-generic 'compute-apply-methods))

(define add-method
  (lambda (generic method)
    (slot-set! generic
	       'methods
	       (cons method
		     (filter-in
		      (lambda (m)
			(not (every eq?
				    (method-specializers m)
				    (method-specializers method))))
		      (slot-ref generic 'methods))))
    (%set-entity-proc! generic (compute-apply-generic generic))))

(define applicable?
  (lambda (c arg)
    (memq c (class-cpl (class-of arg)))))

(define more-specific?
  (lambda (c1 c2 arg)
    (memq c2 (memq c1 (class-cpl (class-of arg))))))

))
