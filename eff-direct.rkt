#lang racket
(require racket/control)

(define (return x) (lambda (k) (k x)))
(define (bind m f) (lambda (k) (m (lambda (v) ((f v) k)))))

(define (reify thunk)
  (bind (reset (return (thunk)))
        (lambda (x) (return x))))
 
(define (reflect m)
  (shift k (bind m k)))

(struct E (r))
(struct V (v))

(define (send f) (reflect (lambda (k) (E (f k)))))
(define (admin m) ((reify m) V))

(define (fmap f m)
  (match m
    [(Reader k) (Reader (compose f k))]
    [(State s k) (State s (compose f k))]))

(define (run m)
  (match m
    [(V x) x]))

(define (relay u)
  (send (lambda (k) (fmap k u))))

(struct Reader (k))
(define (ask) (send Reader))

(define (runReader m e)
  (define (loop m)
    (match m
      [(V v) v]
      [(E (Reader k)) (loop (k e))]
      [(E u) (loop (relay u))]))
  (loop (admin m)))

(define (t1) (+ (ask) 1))
(runReader t1 10)

(struct State (t k))

(define (put s) (send (lambda (k) (State (lambda (_) s) (lambda (_) (k '()))))))
(define (modify f) (send (lambda (k) (State f k))))
(define (get) (send (lambda (k) (State (lambda (s) s) k))))

(define (runState m s)
  (define (loop s m)
    (match m
      [(V v) (cons v s)]
      [(E (State t k)) (let ((s1 (t s)))
                         (loop s1 (k s1)))]
      [(E u) (loop s (relay u))]))
  (loop s (admin m)))

(define (ts1) (put 10) (get))
(runState ts1 0)


(define-syntax with-handler
  (syntax-rules ()
    ((_ (h r ...) b ...) (h (lambda () b ...) r ...))))

(with-handler (runState 10)
  (put 10)
  (get))

(with-handler (runReader 3)
  (with-handler (runState 1)
    (with-handler (runReader 2)
      (+ (get)
         (ask)))))

(with-handler (runReader 3)
  (with-handler (runState 1)
    (put (+ (ask) (with-handler (runReader 4) (get))))
    (get)))
