#lang racket

(require 2htdp/image
         (for-syntax racket))

(provide generate-bar-code)

(define (image-apply image thunks)
  (if (null? thunks)
      image
      (image-apply ((car thunks) image) (cdr thunks))))

(define white-color (make-color 255 255 255))
(define black-color (make-color 0 0 0))

(define (generate-bar-code digits)
  (define bit-sequence-length 7)
  (define bit-sequence-amount 12)
  (define bits-in-barcode 95)
  (define side-guard-pattern '(1 0 1))
  (define middle-guard-pattern '( 0 1 0 1 0))
  (define height 1)
  (define (bit-sequence->value bit-sequence side)
    (define-values (left-code right-code)
      (match bit-sequence
        [0
         (values '(0 0 0 1 1 0 1) '(1 1 1 0 0 1 0))]
        [1
         (values '(0 0 1 1 0 0 1) '(1 1 0 0 1 1 0))]
        [2
         (values '(0 0 1 0 0 1 1) '(1 1 0 1 1 0 0))]
        [3
         (values '(0 1 1 1 1 0 1) '(1 0 0 0 0 1 0))]
        [4
         (values '(0 1 0 0 0 1 1) '(1 0 1 1 1 0 0))]
        [5
         (values '(0 1 1 0 0 0 1) '(1 0 0 1 1 1 0))]
        [6
         (values '(0 1 0 1 1 1 1) '(1 0 1 0 0 0 0))]
        [7
         (values '(0 1 1 1 0 1 1) '(1 0 0 0 1 0 0))]
        [8
         (values '(0 1 1 0 1 1 1) '(1 0 0 1 0 0 0))]
        [9
         (values '(0 0 0 1 0 1 1) '(1 1 1 0 1 0 0))]))
    (match side
      ['left left-code]
      ['right right-code]))
  (define (append-bit current-image bit)
    (beside current-image
            (rectangle 1 1 'solid (if (zero? bit)
                                      white-color
                                      black-color))))
  (define (append-bit-sequence current-image bit-seq)
    (foldl (lambda (bit image)
             (append-bit image bit))
           current-image
           bit-seq))
  (define (add-side-guard current-image)
    (foldl (lambda (bit image)
             (append-bit image bit))
           current-image
           side-guard-pattern))
  (define (add-middle-guard current-image)
    (foldl (lambda (bit image)
             (append-bit image bit))
           current-image
           middle-guard-pattern))
  (define (append-number image digit side)
    (append-bit-sequence image
                         (bit-sequence->value digit side)))
  (let ([base-image (rectangle 0 1 'solid (make-color 255 255 255))])
    (add-side-guard (foldl (lambda (digit image)
                             (append-number image digit 'right))
                           (add-middle-guard (foldl (lambda (digit image)
                                                      (append-number image digit 'left))
                                                    (add-side-guard base-image)
                                                    (take digits 6)))
                           (drop digits 6)))))
