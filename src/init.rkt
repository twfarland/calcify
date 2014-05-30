#lang racket

(provide init)

(define (init)
  (for ((dir (list "public"
                   "assets"
                   "assets/css"
                   "assets/js"
                   "assets/img"
                   "content"
                   "views")))
    (and (not (directory-exists? dir)) (make-directory dir))))