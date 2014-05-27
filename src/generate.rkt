#lang racket

(require "utils.rkt" "build-content.rkt")


;; string -> void
;; creates directories in file path if they don't exist
(define (write-path path content)
  
  (define (aux root parts)
    (if (= (length parts) 1) (display-to-file content path)
      (let ((root (++ root "/" (car parts))))
        (if (directory-exists? root) 
            (aux root (cdr parts))
            (begin (make-directory root) (aux root (cdr parts)))))))
  
  (aux "" (string-split path "/")))
    

;; void -> void
;; generate the site static files from content
(define (generate)
  
  (delete-directory/files "public")
  (copy-directory/files "assets" "public")
  
  (define here (path->string (current-directory)))
  (define content (build-content here))
  
  (for ((route-html content))
    (match route-html
      ((list route html) (write-path (++ here "public/" route "/index.html") html))))
  
  (display (++ "Inert: built [" (number->string (length content)) "] pages\n")))


(generate)




