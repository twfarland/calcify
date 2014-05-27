#lang racket

  
(define (test-string str1 str2)
  (list (string=? str1 str2) str1 str2))

(define test-list
  (list (test-string (sxp->html `(tag)) "<tag>")
        (test-string (sxp->html `(tag text)) "<tag>text</tag>")
        (test-string (sxp->html `(tag (= attr 1) text)) "<tag attr=\"1\">text</tag>")
        (test-string (sxp->html `(tag (= attr val))) "<tag attr=\"val\">")
        (test-string (sxp->html `(tag "")) "<tag></tag>")
        (test-string (sxp->html `(tag (tag2 text) (tag3 (tag4)))) "<tag><tag2>text</tag2> <tag3><tag4></tag3></tag>")
        (test-string (sxp->html `((tag) (tag2))) "<tag><tag2>")
        (test-string (sxp->html `(tag word1 "word2" 3)) "<tag>word1 word2 3</tag>")))

(filter (compose not car) test-list)
