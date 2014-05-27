#lang racket

(require "utils.rkt")
(provide sxp->html)


;; [[key val] | (hash key val)], _ -> _ | ""
(define (scope-ref scope k)
  (list-any scope (λ (coll) (ref coll k))))


;; sxp -> string
(define (sxp->attr sxp scope) 
  (++ (sxp->html (cadr sxp) scope) "=\"" (sxp->html (caddr sxp) scope) "\""))


;; sxp -> bool
;; given an s-expression, return whether it is an attribute pair, e.g: (= attr val)
(define (attr? sxp)
  (and (cons? sxp) (symbol=? (car sxp) '=)))


;; sxp -> string
;; convert an s-expression into an html tag
(define (sxp->tag sxp scope)
  
  (define-values (attrs children) (partition attr? (cdr sxp)))
  (define tag (symbol->string (car sxp)))
  
  (let ((attrs (if (null? attrs)    "" (string-join (cons "" (map (λ (s) (sxp->attr s scope)) attrs)) " ")))
        (body  (if (null? children) "" (string-join (map (λ (s) (sxp->html s scope)) children) " ")))
        (close (if (null? children) "" (++ "</" tag ">"))))
    (string-append "<" tag attrs ">" body close)))


;; (? prop then) if exists then then or ""
;; (? prop then else) if exits then or else
(define (sxp-if sxp scope)
  (to-string (match sxp
               ((list prop then) (if (scope-ref scope prop) then ""))
               ((list prop then else) (if (scope-ref scope prop) then else)))))


;; (-> (props val i) body)
(define (sxp-for sxp scope)
  (to-string (match sxp
               ((list (list props v i) body) 
                (map-iter (λ (v2 i2) (sxp->html body (cons (hash v v2 i i2) scope)))
                          (scope-ref scope props))))))


; (: prop) if exists, or ""
; (: prop k1 ... kn) if exists, or ""
(define (sxp-ref sxp scope)
  (define (aux val keys) 
    (if (null? keys) 
        (sxp->html val scope) 
        (aux (ref val (car keys)) (cdr keys))))
  (aux (scope-ref scope (car sxp)) (cdr sxp)))
  

;; sxp -> string    
;; convert an s-expression into an html string
(define (sxp->html sxp scope)
  (cond ((cons? sxp) (let ((x (car sxp)))
                       (cond ((cons? x) (apply ++ (map (λ (s) (sxp->html s scope)) sxp)))
                             ((symbol=? x ':)  (sxp-ref (cdr sxp) scope))
                             ((symbol=? x '->) (sxp-for (cdr sxp) scope))
                             ((symbol=? x '?)  (sxp-if  (cdr sxp) scope))
                             (else (sxp->tag sxp scope)))))
        (else (to-string sxp))))




