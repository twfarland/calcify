#lang racket

(define version "0.0.1\n")

(require "develop.rkt"
         "generate.rkt"
         "init.rkt")

(let ((args (current-command-line-arguments)))
  
  (if (= (vector-length args) 1)
      
      (match (vector-ref args 0)
        
        ("-dev" (develop))
        ("-init" (init))
        ("-v" (display version))
        (_ (display (string-append "\nUsage: calcify [options]\n\n"
                                   
                                   "With zero option flags, calcify generates static files in {CURRENT_DIRECTORY}/public\n\n"
                                   
                                   "Options:\n"
                                   "   -v       Print calcify's version\n"
                                   "   -h       Display this help message\n"
                                   "   -init    Initialize calcify project folders in the current directory\n"
                                   "   -dev     Run the project in the current directory locally\n\n"))))
      
      (generate)))