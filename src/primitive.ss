#lang scheme/base
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARSEQ.PLT 
;; A Parser Combinator library.
;; 
;; Bonzai Lab, LLC.  All rights reserved.
;; 
;; Licensed under LGPL.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; primitive.ss - holds the primitive parsers...  
;; yc 12/31/2009 - first version 
;; yc 1/5/2010 - added literal & literal-ci 
;; yc 1/18/2010 - move make-reader to reader.ss 
;; yc 8/2/2010 - add char-not-part-of-chars and char-not-part-of-string 

(require "depend.ss"
         "util.ss"
         "input.ss" 
         ) 

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; primitive parsers
(define (identity n) n)
;; return 
(define (return v (size 0)) 
  (lambda (in)
    (values v
            (new-input in size))))

;; struct failed - represents failed parse... 
(define-struct failed (pos) #:prefab)

;; succeeded? 
(define (succeeded? v) (not (failed? v))) 

;; fail - the parser that returns failed with the current port position. 
(define (fail in) 
  (values (make-failed (input-pos in))
          in)) 

;; SOF (start-of-file) 
;; returns true only when the input-pos = 0
(define (SOF in) 
  ((if (= (input-pos in) 0)
       (return 'sof)
       fail) in)) 

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; item-based primitive parsers 

;; item 
;; the fundamental building block 
(define (item peek isa? satisfy? size) 
  (lambda (in) 
    (let ((v (peek in))) 
      ((if (and (isa? v) (satisfy? v))
           (return v (size v))
           fail) in))))

;; bytes=
;; parses if the next part of the input matches the exact bytes
(define (bytes= bytes) 
  (let ((size (bytes-length bytes)))
    (item (peek-bytes* size) 
          bytes?
          (lambda (b) 
            (bytes=? b bytes))
          (the-number size))))

;; string=
;; parses if the next part of the input matches the exact string
(define (string= s (comp? string=?))
  (let ((size (string-bytes/utf-8-length s)))
    (item (peek-string* size)
          string?
          (lambda (str)
            (comp? str s))
          (the-number size))))

(define (string-ci= s) 
  (string= s string-ci=?))

;; byte-when 
;; return the next byte when satisfy matches
(define (byte-when satisfy? (isa? byte?) (size (the-number 1)))
  (item peek-byte* isa? satisfy? size))

;; any-byte 
;; return the next byte 
(define any-byte (byte-when (lambda (n) n)))

;; byte= 
(define (byte= b) (byte-when (lambda (v)
                               (= b v)))) 

;; EOF 
;; return if the next byte is eof 
(define EOF (byte-when (lambda (n) n) eof-object? (the-number 0)))

;; bits= 
;; matches a byte @ the bits level... (pass in the individual bits) 
(define (bits= bits)
  (byte-when (lambda (b) (= b (bits->byte bits)))))

;; byte-in 
(define (byte-in bytes) 
  (byte-when (lambda (b) (member b bytes))))

(define (byte-not-in bytes)
  (byte-when (lambda (b) (not (member b bytes)))))

(define (byte-between lb hb)
  (byte-when (lambda (b) (<= lb b hb)))) 

(define (byte-not-between lb hb)
  (byte-when (compose not (lambda (b) (<= lb b hb)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; character-based parsers 

;; char-when 
;; the fundamental character-based parser 
(define (char-when satisfy?)
  (item peek-char* char? satisfy? char-utf-8-length)) 

;; any-char 
;; return the next character 
(define any-char (char-when (lambda (n) n)))

;; char= 
;; return the next character if it equals c 
(define (char= c (comp? char=?) (trans identity)) 
  (char-when (lambda (v) (trans (comp? c v)))))

;; char-ci= 
(define (char-ci= c) (char= c char-ci=?))

;; char-not 
;; return the next character if it is not c 
(define (char-not= c (comp? char=?)) (char= c comp? not))

;; char-ci-not
(define (char-ci-not= c) (char-not= char-ci=?))

;; char-between 
;; return the next character if it falls in between lc & hc 
(define (char-between lc hc (comp? char<=?) (trans identity)) 
  (char-when (lambda (v) (trans (comp? lc v hc)))))

;; char-ci-between
(define (char-ci-between lc hc) (char-between lc hc char-ci<=?))

(define (char-not-between lc hc (comp? char<=?))
  (char-between lc hc comp? not))

;; char-ci-not-between 
(define (char-ci-not-between lc hc) (char-not-between lc hc char-ci<=?))

;; char-in 
;; return the next character if it one of the chars 
(define (char-in chars (comp? char=?) (trans identity))
  (char-when (lambda (v) 
               (trans (memf (lambda (c)
                              (comp? c v)) 
                            chars)))))

;; char-ci-in 
(define (char-ci-in chars) (char-in chars char-ci=?))

;; char-not-in 
;; return the next character if it is not one of the characters 
(define (char-not-in chars (comp? char=?))  (char-in chars comp? not))

;; char-ci-not-in 
(define (char-ci-not-in chars) (char-not-in chars char-ci=?))

;; literal 
;; returns a parser based on the passed in literal 
(define (literal p) 
  (cond ((char? p) (char= p)) 
        ((byte? p) (byte= p)) 
        ((string? p) (string= p)) 
        ((bytes? p) (bytes= p)) 
        (else p))) 

;; literal-ci 
;; a ci version of literal 
(define (literal-ci p) 
  (cond ((char? p) (char-ci= p)) 
        ((string? p) (string-ci= p)) 
        (else (literal p)))) 

;; what is the or/c
(define Literal/c (or/c string? bytes? char? byte?))

(define Literal-Parser/c (or/c Literal/c Parser/c))

;; char-not-part-of-chars 
;; allows for using with delimiters that are longer than a single character. 
(define (char-not-part-of-chars . chars) 
  (define (helper rest in fail first)
    (if (null? rest)
        (fail)
        (let-values (((c IN)
                      (peek-char*/incr in))) 
          (if (and (char? c) (char=? (car rest) c)) 
              (helper (cdr rest) IN fail first)
              (first)))))
  (define (first-helper in)
    (let-values (((c IN) 
                  (peek-char*/incr in))) 
      (cond ((eof-object? c)
             (fail IN)) 
            ((char=? (car chars) c)
             (helper (cdr chars) 
                     IN 
                     (lambda ()
                       (fail in)) 
                     (lambda ()
                       (values c IN))))
            (else 
             (values c IN)))))
  (cond ((null? chars) 
         any-char)
        ((null? (cdr chars))
         (char-not-in chars))
        (else first-helper)))

;; char-not-part-of-string 
;; passing in a string instead of a list of chars. 
(define (char-not-part-of-string str)
  (apply char-not-part-of-chars (string->list str))) 

(provide return
         (struct-out failed) 
         succeeded? 
         fail
         SOF
         item
         bytes=
         string=
         string-ci= 
         byte-when
         any-byte
         byte=
         EOF
         bits=
         byte-in
         byte-not-in
         byte-between
         byte-not-between
         char-when
         any-char
         char=
         char-ci=
         char-not=
         char-ci-not=
         char-between
         char-ci-between
         char-not-between
         char-ci-not-between
         char-in
         char-ci-in 
         char-not-in
         char-ci-not-in 
         literal 
         literal-ci 
         Literal/c
         Literal-Parser/c
         char-not-part-of-chars
         char-not-part-of-string
         )
