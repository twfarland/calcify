#lang racket

(require "utils.rkt" "sxp-html.rkt")
(provide build-content)

(define pwd (current-directory))

;; string -> [string]
;; gets a list of valid content filenames from a directory
(define (dir->content dir)
  (filter (curry regexp-match (regexp ".rkt$")) 
          (map path->string (directory-list dir))))


;; string -> (hash string view)
(define (get-views dir)
  (for/hash ((filename (dir->content dir)))
    (values (string-replace filename ".rkt" "")
            (file->value (++ dir "/" filename)))))


;; string -> [[filename page]]
(define (get-content dir)
  (map (λ (filename) 
         (list filename (assoc-list->hash (file->value (++ dir "/" filename)))))
       (dir->content dir)))


;; void -> [[route html]]
(define (build-content dir)
  (define views (get-views (++ dir "views")))
  (map (match-lambda
         ((list filename page) (list (@ page 'route)
                                     (sxp->html (@ views (@ page 'view)) (list page))))) 
       (get-content (++ dir "content"))))

