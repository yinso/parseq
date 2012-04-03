#lang scheme/base
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARSEQ.PLT 
;; A Parser Combinator library.
;; 
;; Bonzai Lab, LLC.  All rights reserved.
;; 
;; Licensed under LGPL.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; input.ss - holds the abstraction of the input object... 
;; yc 12/31/2009 - first version 
;; yc 1/8/2009 - fix build-input & Input/c 
(require "depend.ss")
;; state 
;; the struct that abstracts the input 
;; currently this holds an input-port + the position on the port
;; in the future this can be used to hold string, list, vector, etc. 
(define-struct input (source bytes lines chars) #:prefab) ;; by default 

;; input
;; an utility for converting source into input state.
(define (build-input v (pos 0))
  (define (helper v)
    (cond ((input-port? v) v)
          ((string? v) (open-input-string v)) 
          ((bytes? v) (open-input-bytes v))))
  (if (input? v) 
      (new-input v pos)
      (make-input (helper v) pos 0 0))) 

;; new-input 
;; make a new input based on the old input and a new position... 
;; how do we make the input lines & chars to be automatically incrmented? 
;; first of all - we do not have a knowledge about lines or chars... this needs to be done @ the char-test level? 
(define (new-input input incr (new-line 0))
  (make-input (input-source input) 
              (+ incr (input-bytes input))
              (+ new-line (input-lines input))
              (if (> new-line 0) 0 (+ incr (input-chars input)))))

;; peek-bytes* 
;; return a funtion that will make a particular amount of reading based on 
;; the requested size... 
(define (peek-bytes* size) 
  (lambda (in)
    (peek-bytes size (input-bytes in) (input-source in))))

;; peek-string* 
;; return a function that will read a particular size of string... 
;; this can fail since it is expected to be using utf-8 as the input size... 
(define (peek-string* size) 
  (lambda (in) 
    (peek-string size (input-bytes in) (input-source in))))

;; peek-byte*
;; peek a single byte 
(define (peek-byte* in)
  (peek-byte (input-source in) (input-bytes in)))

;; peek-char*
;; peek a single char
(define (peek-char* in)
  (peek-char (input-source in) (input-bytes in)))

(define (peek-char*/incr in) 
  (let ((c (peek-char* in)))
    (if (char? c)
        (values c 
                (new-input in (char-utf-8-length c)))
        (values c in) ;; eof 
        )))

;; read-bytes* 
;; read out the bytes based on the size of the input... 
(define (read-bytes* in)
  (read-bytes (input-bytes in) (input-source in)))

(define Input/c (or/c input? bytes? string? input-port?))

(define Parser/c (-> Input/c (values any/c Input/c)))

(provide input
         input?
         input-source
         input-bytes
         (rename-out (build-input make-input))
         new-input
         peek-bytes*
         peek-string*
         peek-byte*
         peek-char*
         peek-char*/incr
         read-bytes*
         Input/c
         Parser/c 
         )