;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; virgo/misc.sld - misc, missing procedures or so
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

(define-library (virgo misc)
  (export list-sort
	  last-pair
	  list*
	  apply*
	  position-of
	  map-append
	  every
	  remove
	  getl
	  union
	  filter-in
	  collect-if
	  remove-duplicates
	  print)
  (import (scheme base) (scheme write))
  (begin
;; for debug
(define (print . args) (for-each display args) (newline))

(define (take lis k)
  (let recur ((lis lis) (k k))
    (if (zero? k)
	'()
	(cons (car lis)
	      (recur (cdr lis) (- k 1))))))

;; from Sagittarius
(define (list-sort proc lst)
  (define (merge-list! proc head lst1 lst2 tail)
    (let loop ()
      (cond ((proc (car lst2) (car lst1))
	     ;; we can't use macro so duplicate it!
	     (set-cdr! tail lst2)
	     (set! tail lst2)
	     (let ((rest (cdr lst2)))
	       (cond ((null? rest)
		      (set-cdr! lst2 lst1)
		      (cdr head))
		     (else
		      (set! lst2 rest)
		      (loop)))))
	    (else
	     (set-cdr! tail lst1)
	     (set! tail lst1)
	     (let ((rest (cdr lst1)))
	       (cond ((null? rest)
		      (set-cdr! lst1 lst2)
		      (cdr head))
		     (else
		      (set! lst1 rest)
		      (loop))))))))
  (define (fast-merge-list! proc try? head lst1 tail1 lst2 tail2 rest)
    (if try?
	(cond ((not (proc (car lst2) (car tail1)))
	       (set-cdr! tail1 lst2)
	       (values lst1 tail2 rest))
	      ((proc (car tail2) (car lst1))
	       (set-cdr! tail2 lst1)
	       (values lst2 tail1 rest))
	      (else 
	       (values (merge-list! proc head lst1 lst2 head)
		       (if (null? (cdr tail1))
			   tail1
			   tail2)
		       rest)))
	(values (merge-list! proc head lst1 lst2 head)
		(if (null? (cdr tail1))
		    tail1
		    tail2)
		rest)))
  (define (do-sort lst size head)
    (define (recur lst size)
      (cond ((= size 1)
	     (let ((h (list (car lst))))
	       (values h h (cdr lst))))
	    ((= size 2)
	     (let* ((a (car lst))
		    (ad (cadr lst))
		    (h (if (proc ad a)
			   (list ad a)
			   (list a ad))))
	       (values h (cdr h) (cddr lst))))
	    (else
	     (let ((half (quotient size 2)))
	       (let*-values (((lst1 tail1 rest) (recur lst half))
			     ((lst2 tail2 rest) (recur rest (- size half))))
		 (fast-merge-list! proc (>= size 8) head
				   lst1 tail1
				   lst2 tail2
				   rest))))))
    (let-values (((lst tail size) (recur lst size)))
      lst))
  (define (divide lst)
    (let loop ((acc 1) (lst lst))
      (cond ((null? (cdr lst)) (values acc '()))
            (else
	     (if (proc (car lst) (cadr lst))
		 (loop (+ acc 1) (cdr lst))
		 (values acc (cdr lst)))))))
  (unless (procedure? proc)
    (error "list-sort: procedure required for first argument" proc))

  (if (null? lst)
      lst
      (let-values (((n lst2) (divide lst)))
	(if (null? lst2)
	    lst
	    (let* ((head (cons '() '()))
		   (r (do-sort lst2 (length lst2) head)))
	      (merge-list! proc head (take lst n) r head))))))

(define (last-pair lis)
  (let loop ((lis lis))
    (let ((tail (cdr lis)))
      (if (pair? tail) (loop tail) lis))))

;; below copyright
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
(define list*
  (lambda args
    (letrec ((chase
	      (lambda (args)
		(cond ((null? args) '())
		      ((null? (cdr args)) (car args))
		      (else (cons (car args) (chase (cdr args))))))))
      (chase args))))

(define apply*
  (lambda (proc . args)
    (apply proc (apply list* args))))


(define position-of
  (lambda (x lst)
    (if (eq? x (car lst)) 0 (+ 1 (position-of x (cdr lst))))))

(define map-append
  (lambda (proc . lists)
    (apply append (apply map (cons proc lists)))))

(define every
  (lambda (test . lists)
    (let scan ((tails lists))
      (if (member #t (map null? tails))             ;(any null? lists)
	  #t
	  (and (apply test (map car tails))
	       (scan (map cdr tails)))))))

(define remove
  (lambda (x list)
    (cond ((null? list) '())
	  ((eq? (car list) x) (cdr list))
	  (else (cons (car list) (remove x (cdr list)))))))

(define getl
  (lambda (initargs name . not-found)
    (letrec ((scan (lambda (tail)
		     (cond ((null? tail)
			    (if (pair? not-found)
				(car not-found)
				(error "GETL couldn't find" name)))
			   ((eq? (car tail) name) (cadr tail))
			   (else (scan (cddr tail)))))))
      (scan initargs))))

(define union
  (lambda lists
    (letrec ((clean (lambda (list result)
		      (cond ((null? list) result)
			    ((memq (car list) result)
			     (clean (cdr list) result))
			    (else
			     (clean (cdr list) (cons (car list) result)))))))
      (clean (apply append lists) '()))))

(define filter-in
  (lambda (f l)
    (cond ((null? l) '())
	  ((f (car l)) (cons (car l) (filter-in f (cdr l))))
	  (else (filter-in f (cdr l))))))

(define collect-if
  (lambda (test? list)
    (cond ((null? list) '())
	  ((test? (car list)) (cons (car list) (collect-if test? (cdr list))))
	  (else (collect-if test? (cdr list))))))

(define remove-duplicates
  (lambda (list)
    (let loop ((result-so-far '())
	       (remaining list))
      (if (null? remaining)
	  result-so-far
	  (if (null? (memq (car remaining) result-so-far))
	      (loop (cons (car remaining) result-so-far)
		    (cdr remaining))
	      (loop result-so-far
		    (cdr remaining)))))))


))
