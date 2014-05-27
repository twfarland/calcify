#lang racket

;; Run a simple local static file server
;; rebuilds the content hash on each request
;; obviously this is slow, but is just used as a development convenience
;; to avoid the need for regenerating static files after each change

(require web-server/servlet
         web-server/servlet-env
         "build-content.rkt"
         "utils.rkt")


;; This needs to be defined outside the scope of (start)
(define dir (path->string (current-directory)))


;; Response conveniences
(define (ok bs type)
  (response 200 #"OK"
            (current-seconds) (string->bytes/utf-8 type) null
            (curry write-bytes bs)))

(define (not-found)
  (response 404 #"Not Found" (current-seconds) #f null void))


;; Reponse handler
(define (start req)
  
  (define content 
    (ref (assoc-list->hash (build-content dir))
         (string-trim (url->string (request-uri req)) "/")))
  
  (if content 
      (ok (string->bytes/utf-8 content) "text/html")
      (not-found)))


;; Server config
;; Handles css,js,img directories normally
(serve/servlet start 
               #:port 8080
               #:servlet-path "/"
               #:servlet-regexp (regexp "(?<!\\.js|css|jpg|png|gif)$")
               #:extra-files-paths (list (build-path dir "assets"))
               #:stateless? #t)



