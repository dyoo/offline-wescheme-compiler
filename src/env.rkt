#lang planet dyoo/whalesong

(require "rbtree.rkt"
         "helpers.rkt"
         "stx.rkt"
         "binding.rkt")

;; An env collects a set of bindings.
(define-struct env (bindings))
(define empty-env (make-env empty-rbtree))


(define first car)
(define second cadr)


;; env-extend: env binding -> env
(define (env-extend an-env new-binding)
  (cond
    [(binding:constant? new-binding)
     (make-env (rbtree-insert symbol< 
                              (env-bindings an-env) 
                              (binding-id new-binding)
                              new-binding))]
    [(binding:function? new-binding)
     (make-env (rbtree-insert symbol< 
                              (env-bindings an-env) 
                              (binding-id new-binding)
                              new-binding))]
    [(binding:structure? new-binding)
     (make-env (rbtree-insert symbol< 
                              (env-bindings an-env) 
                              (binding-id new-binding)
                              new-binding))]))



;; env-lookup: env symbol -> (or/c binding false)
(define (env-lookup an-env name)
  (define result (rbtree-lookup symbol< (env-bindings an-env) name))
  (cond [(pair? result)
         (cadr result)]
        [else
         #f]))


;; env-contains?: env symbol -> boolean
(define (env-contains? an-env name)
  (binding? (env-lookup an-env name)))
    


;; env-keys: env -> (listof symbol)
;; Produces the keys in the environment.
(define (env-keys an-env)
  (map first (rbtree->list (env-bindings an-env))))




;; env-extend-constant: env (module-path | #f) symbol string -> env
;; Extends the environment with a new constant binding.
(define (env-extend-constant an-env id module-source)
  (env-extend an-env
              (make-binding:constant id module-source '())))


;; env-extend-function: env symbol (or/c string false) number boolean? string? -> env
;; Extends the environment with a new function binding
(define (env-extend-function an-env id module-source min-arity var-arity?)
  (env-extend an-env
              (make-binding:function id 
                                     module-source
                                     min-arity 
                                     var-arity?
                                     '()
                                     #f)))


;; env-lookup/context: identifier-stx -> (binding | false)
;; Lookup an identifier, taking into account the context of the identifier.  If it has no existing
;; context, look at the given an-env.
;; In either case, either return a binding, or false.
(define (env-lookup/context an-env an-id-stx)
  (cond
    [(env? (stx-context an-id-stx))
     (cond [(not (env-contains? (stx-context an-id-stx) (stx-e an-id-stx)))
            #f]
           [else
            (env-lookup (stx-context an-id-stx) (stx-e an-id-stx))])]
    [else
     (cond [(not (env-contains? an-env (stx-e an-id-stx)))
            #f]
           [else
            (env-lookup an-env (stx-e an-id-stx))])]))



(provide
 [struct-out env]
 empty-env
 env-extend
 env-lookup
 env-lookup/context
 env-contains?
 env-keys
 
 env-extend-constant
 env-extend-function)