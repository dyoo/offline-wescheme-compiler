#lang racket/base


;; Representation of the stack environment of the mzscheme vm, so we know where
;; things live.

(define-struct env () #:transparent)
(define-struct (empty-env env) () #:transparent)
(define-struct (local-env env) (name boxed? parent-env) #:transparent)
(define-struct (global-env env) (names parent-env) #:transparent)
(define-struct (unnamed-env env) (parent-env) #:transparent)

(define EMPTY-ENV (make-empty-env))



;; env-push-globals: env (listof symbol) -> env
(define (env-push-globals env names)
  (make-global-env names env))


;; env-push-local: env symbol -> env
(define (env-push-local env name)
  (make-local-env name #f env))


;; env-push-local: env symbol -> env
(define (env-push-local/boxed env name)
  (make-local-env name #t env))


;; env-push-unnamed: env -> env
(define (env-push-unnamed env)
  (make-unnamed-env env))


;; env-pop: env -> env
(define (env-pop env)
  (cond
    [(empty-env? env)
     (error 'env-pop "empty env")]
    [(local-env? env)
     (local-env-parent-env env)]
    [(global-env? env)
     (global-env-parent-env env)]
    [(unnamed-env? env)
     (unnamed-env-parent-env env)]))



(define-struct stack-reference () #:transparent)
(define-struct (local-stack-reference stack-reference) (name boxed? depth) #:transparent)
(define-struct (global-stack-reference stack-reference) (name depth pos) #:transparent)
(define-struct (unbound-stack-reference stack-reference) (name) #:transparent)



;; position: symbol (listof symbol) -> (or number #f)
;; Find position of element in list; return false if we can't find the element.
(define (position x L)
  (let loop ([i 0]
             [L L])
    (cond
      [(null? L)
       #f]
      [(eq? x (car L))
       i]
      [else
       (loop (add1 i)
             (cdr L))])))


;; env-lookup: env symbol -> stack-reference
(define (env-lookup env a-name)
  (let loop ([env env]
             [depth 0])
    (cond
      [(empty-env? env)
       (make-unbound-stack-reference a-name)]
      
      [(local-env? env)
       (define name (local-env-name env))
       (define boxed? (local-env-boxed? env))
       (define parent-env (local-env-parent-env env))
       (cond
         [(eq? a-name name)
          (make-local-stack-reference name boxed? depth)]
         [else 
          (loop parent-env (add1 depth))])]
      
      [(global-env? env)
       (define names (global-env-names env))
       (define parent-env (global-env-parent-env env))
       (cond [(position a-name names)
              =>
              (lambda (pos)
                (make-global-stack-reference a-name depth pos))]
             [else
              (loop parent-env (add1 depth))])]
      
      [(unnamed-env? env)
       (loop (unnamed-env-parent-env env) (add1 depth))])))


;; env-peek: env number -> env
(define (env-peek env depth)
  (let loop ([env env]
             [depth depth])
    (cond
      [(= depth 0)
       env]
      [else
       (cond
         [(empty-env? env)
          (error 'env-peek)]
         
         [(local-env? env)
          (loop (local-env-parent-env env) (sub1 depth))]

         [(global-env? env)
          (loop (global-env-parent-env env) (sub1 depth))]
         [(unnamed-env? env)
          (loop (unnamed-env-parent-env env) (sub1 depth))])])))
                 
         

(provide env? 
         (rename-out (EMPTY-ENV empty-env))
         env-push-globals
         env-push-local
         env-push-local/boxed
         env-push-unnamed
         env-pop
         
         env-lookup
         
         env-peek
         
         (struct-out global-env)
         
         (struct-out stack-reference)
         (struct-out local-stack-reference)
         (struct-out global-stack-reference)
         (struct-out unbound-stack-reference))
