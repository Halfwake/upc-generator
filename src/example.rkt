#lang racket

(require "upc.rkt"
         2htdp/image)

(define result (generate-bar-code '(0 5 1 0 0 0 0 1 2 5 1 7)))
result
(newline)
(scale/xy 1 60 result)