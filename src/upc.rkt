#lang racket

(require 2htdp/image
         (for-syntax racket))

(provide generate-bar-code)

(define (image-apply image thunks)
  (if (null? thunks)
      image
      (image-apply ((car thunks) image) (cdr thunks))))

(define-syntax (image-pipe stx)
  (datum->syntax stx
                 (match (syntax->datum stx)
                   [(list _ image thunks ...)
                    `(image-apply ,image (list ,@(map (lambda (thunk)
                                                        `(lambda (image)
                                                           ,thunk))
                                                      thunks)))])))

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
  (define (start-position-index position)
    (sub1 (if (> position 5)
              (+ (length side-guard-pattern) (* bit-sequence-length position))
              (+ (length side-guard-pattern)
                 (* bit-sequence-length (/ bit-sequence-amount 2))
                 (length middle-guard-pattern)
                 (* (- position 7) bit-sequence-length)))))
  (define (place-bit image position)
    (scene+line image position 0 position height (make-color 0 0 0)))   
  (define (place-bit-sequence image bit-seq start-index)
    (foldl (lambda (i image)
             (define position (+ start-index i))
             (if (zero? (list-ref bit-seq i))
                 image
                 (place-bit image position)))
           image
           (range (length bit-seq))))
  (define (add-side-guards image)
    (image-pipe
     (place-bit image 0)
     (place-bit image 2)
     (place-bit image (- bits-in-barcode 1))
     (place-bit image (- bits-in-barcode 3))))
  (define (add-middle-guard image)
    (define middle-guard-index 45)
    (image-pipe
     (place-bit image (+ middle-guard-index 1))
     (place-bit image (+ middle-guard-index 3))))
  (define (add-guards image)
    (add-middle-guard (add-side-guards image)))
  (define (add-number image value position)
    (define side (if (> position 5) 'right 'left))
    (place-bit-sequence image
                        (bit-sequence->value value side)
                        (start-position-index position)))
  (define (add-numbers image digits)
    (foldl (lambda (i image)
             (add-number image (list-ref digits i) i))
           image
           (range bit-sequence-amount)))
  (let ([base-image (rectangle bits-in-barcode height 'solid (make-color 255 255 255))])
    (add-numbers (add-guards base-image) digits)))