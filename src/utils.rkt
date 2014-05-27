#lang racket


(provide ++ 
         @ 
         list-any 
         to-string 
         ref
         map-iter
         assoc-list->hash)


(define @ hash-ref)


;; string ... -> string
(define ++ string-append)


;; [a], (a -> ~#t | #f) -> ~#t | #f
(define (list-any ls f)
  (if (null? ls) #f
      (or (f (car ls)) (list-any (cdr ls) f))))


;; number | symbol | string -> string
(define (to-string s)
  (cond ((number? s) (number->string s))
        ((symbol? s) (symbol->string s))
        ((list? s) (string-join (map to-string s) " "))
        ((string? s) s)
        (else "")))


;; look for a key in a list or a hash, returning #f if that key not found
;; [_] | (hash symbol _), symbol -> _ | #f
(define (ref coll k)
  (if (list? coll)
      (and (regexp-match (regexp "[0-9]+") (to-string k)) ; key must be integer
           (>= (- (length coll) 1) k) ; and in range
           (list-ref coll k))
      (and (hash-has-key? coll k) ; hash must have key
           (hash-ref coll k))))


;; maps a list, providing the index to the callback
(define (map-iter f ls)
  (define (aux ls i)
    (if (null? ls) ls
        (cons (f (car ls) i) (aux (cdr ls) (+ i 1)))))
  (aux ls 0))


;; makes a hash from an associative list of form: `((key val) (key2 val2) (keyN valN))
(define (assoc-list->hash ls)
  (for/hash ((i ls))
    (values (car i) (cadr i))))
      