;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; virgo/core.sld - Modularised Tiny CLOS
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

(define-library (virgo core)
  (export make class-of slot-ref slot-set!
	  class-direct-slots class-direct-supers class-slots class-cpl
	  generic-methods
	  method-specializers method-procedure
	  
	  make make-class make-generic make-method

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
	  <string> <procedure> <port> <bytevector> <unknown>

	  print-object display-object write-object
	  )
  (import (scheme base)
	  (scheme write)
	  (virgo bootstrap)
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
(define make
  (lambda (class . initargs)
    (let ((instance (allocate-instance class)))
      (initialize instance initargs)
      instance)))

(define (display-object o . maybe-port)
  (let ((port (if (null? maybe-port) 
		  (current-output-port)
		  (car maybe-port))))
    (print-object o port #f)))
(define (write-object o . maybe-port)
  (let ((port (if (null? maybe-port) 
		  (current-output-port)
		  (car maybe-port))))
    (print-object o port #t)))

(define print-object (make-generic 'print-object))

(add-method print-object
  (make-method (list <top> <port> <boolean>)
    (lambda (call-next-method object port write?)
      (if write?
	  (write object port)
	  (display object port)))))

))
