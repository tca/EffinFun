#lang racket
(require racket/control)

(define (cont-return x) (lambda (k) (k x)))
(define (cont-bind m f) (lambda (k) (m (lambda (v) ((f v) k)))))

(define (fmap f m)
  (match m
    [(Reader k) (Reader (compose f k))]
    [(State s k) (State s (compose f k))]))

(struct E (r))
(struct V (v))

(define (send f) (lambda (k) (E (f k))))
(define (admin m) (m V))

(define (run m)
  (match m
    [(V x) x]))

(define (relay u loop)
  (cont-bind (send (lambda (k) (fmap k u))) loop))

(struct Reader (k))
(define (ask) (send Reader))

(define (runReader m e)
  (define (loop m)
    (match m
      [(V x) (cont-return x)]
      [(E (Reader k)) (loop (k e))]
      [(E u) (relay u loop)]))
  (loop (admin m)))

(define t1 (cont-bind (ask) (lambda (x) (cont-return (+ x 1)))))
((runReader t1 10) identity)

(struct State (t k))

(define (put s) (send (lambda (k) (State (lambda (_) s) (lambda (_) (k '()))))))
(define (modify f) (send (lambda (k) (State f k))))
(define (get) (send (lambda (k) (State (lambda (s) s) k))))

(define (runState m s)
  (define (loop s m)
    (match m
      [(V v) (cont-return (cons v s))]
      [(E (State t k)) (let ((s1 (t s)))
                         (loop s1 (k s1)))]
      [(E u) (relay u (curry loop s))]))
  (loop s (admin m)))

(define ts1
    (cont-bind
     (put 10)
     (lambda (_)
       (cont-bind
        (get)
        (lambda (x) (cont-return x))))))

((runState ts1 0) identity)

(define-syntax with-handler
  (syntax-rules ()
    ((_ (h r ...) b ...) (h (begin b ...) r ...))))

((with-handler (runReader 1)
   (with-handler (runState 2)
     (cont-bind
      (ask)
      (lambda (x)
        (cont-bind
         (get)
         (lambda (y)
           (cont-return (+ x y))))))))
 identity)