
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (letrec ([helper
            (lambda (low high stride curr)
              (if (> curr high) null
                  (cons curr (helper low high stride (+ curr stride)))))])
  (helper low high stride low)))

(define (string-append-map xs suffix)
  (let ([f (lambda (x) (string-append x suffix))])
    (map f xs)))

(define (list-nth-mod xs n)
  (cond [(negative? n) (error "list-nth-mod: negative number")]
        [(= (length xs) 0) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (= n 0) null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
  (letrec ([f (lambda (n)
                (if (= (remainder n 5) 0) (cons (- 0 n) (lambda () (f (+ n 1))))
                    (cons n (lambda () (f (+ n 1))))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (n)
                (if (odd? n) (cons "dan.jpg" (lambda () (f (+ n 1))))
                    (cons "dog.jpg" (lambda () (f (+ n 1))))))])
    (lambda () (f 1))))

(define (stream-add-zero s)
  (let ([f (lambda (x)
                (cons 0 (car (x))))])
    (lambda () (cons (f s) (stream-add-zero (cdr (s)))))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (lst1 lst2 n)
                (cons (cons (list-nth-mod lst1 n)
                            (list-nth-mod lst2 n))
                      (lambda () (f lst1 lst2 (+ n 1)))))])
  (lambda () (f xs ys 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (v vec n)
                (cond [(= n (vector-length vec)) #f]
                      [(pair? (vector-ref vec n))
                       (if (= (car (vector-ref vec n)) v) (vector-ref vec n) (f v vec (+ n 1)))]
                      [#t (f v vec (+ n 1))]))])
    (f v vec 0)))

(define (cached-assoc xs n)
  (let* ([cache (make-vector n #f)]
         [index 0]
         [f (lambda (v)
            (let* ([answer (vector-assoc v cache)])
              (if (boolean? answer)
                  (begin (vector-set! cache index (assoc v xs))
                         (if (= index (- n 1))
                             (begin (set! index 0)
                                    (vector-ref cache (- n 1)))
                             (begin (set! index (+ index 1))
                             
                                    (vector-ref cache (- index 1)))))
                  answer)))])
    f))
              
  