#lang planet dyoo/whalesong

(define first car)
(define second cadr)
(define third caddr)
;; Represents the arities available to a function.

(define-struct arity:fixed (n))
(define-struct arity:variable (min max))
(define-struct arity:mixed (arities))

(define (false? f)
  (eq? f #f))

;; arity?: any -> boolean
;; Produces true if X is a procedure arity.
(define (arity? x)
  (or (arity:fixed? x)
      (arity:variable? x)
      (arity:mixed? x)))


;; Translates an arity to an s-expression.
;; arity->sexp: arity -> s-exp
(define (arity->sexp an-arity)
  (cond
    [(arity:fixed? an-arity)
     (list 'arity:fixed (arity:fixed-n an-arity))]
    [(arity:variable? an-arity)
     (list 'arity:variable (arity:variable-min an-arity) (arity:variable-max an-arity))]
    [(arity:mixed? an-arity)
     (list 'arity:mixed (map arity->sexp (arity:mixed-arities an-arity)))]))


;; Translates an s-expression back to an arity.
(define (sexp->arity an-sexp)
  (cond
    [(list? an-sexp)
     (cond
       [(and (eq? (first an-sexp) 'arity:fixed?)
             (= 2 (length an-sexp))
             (number? (second an-sexp)))
        (make-arity:fixed (second an-sexp))]

       [(and (eq? (first an-sexp) 'arity:variable?)
             (= 3 (length an-sexp))
             (or (number? (second an-sexp)) (false? (second an-sexp)))
             (or (number? (third an-sexp)) (false? (third an-sexp))))
        (make-arity:variable (second an-sexp) (third an-sexp))]

       [(and (eq? (first an-sexp) 'arity:mixed?)
             (list (second an-sexp)))
        (define inner-arities (map sexp->arity (second an-sexp)))
        (cond
          [(andmap (lambda (x) (or (arity:fixed? x)
                                   (arity:variable? x)))
                   inner-arities)
           (make-arity:mixed inner-arities)]
          [else
           (error 'sexp->arity 
                  (format "Does not look like an arity structure: ~s" an-sexp))])])]


    [else
     (error 'sexp->arity (format "Does not look like an arity structure: ~s" an-sexp))]))



(provide
 [struct-out arity:fixed]
 [struct-out arity:variable]
 [struct-out arity:mixed]
 arity?)