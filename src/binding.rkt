#lang racket/base

(require "permission-struct.rkt")
(define first car)



;; binding:constant records an id and its associated Java implementation.
(define-struct binding:constant
  (name 
   module-source
   permissions))


;; Function bindings try to record more information about the toplevel-bound
;; function
(define-struct binding:function
  
  (name           ;; name of the function
   module-source  ;; source where the function's really defined
   min-arity      ;; minimal arity to call
   var-arity?     ;; is this vararity?
   permissions    ;; what permissions do we need to call this function?
   cps?           ;; does the function respect CPS calling conventions?
   ))


;; A binding to a structure.
(define-struct binding:structure
  (name        ;; symbol
   module-source
   fields      ;; (listof symbol)
   constructor ;; symbol
   predicate   ;; symbol
   accessors   ;; (listof symbol)
   mutators    ;; (listof symbol)
   permissions ;; (listof permission)
   ))




;; A binding associates a symbol with some value.
;; binding?: any -> boolean
(define (binding? datum)
  (or (binding:constant? datum)
      (binding:function? datum)
      (binding:structure? datum)))



;; binding-id: binding -> symbol
;; Given a binding, produces its identifier.
(define (binding-id a-binding)
  (cond
    [(binding:constant? a-binding)
     (binding:constant-name a-binding)]
    [(binding:function? a-binding)
     (binding:function-name a-binding)]
    [(binding:structure? a-binding)
     (binding:structure-name a-binding)]))


(define (binding-module-source a-binding)
  (cond
    [(binding:constant? a-binding)
     (binding:constant-module-source a-binding)]
    [(binding:function? a-binding)
     (binding:function-module-source a-binding)]
    [(binding:structure? a-binding)
     (binding:structure-module-source a-binding)]))


;; binding->sexp: binding -> s-expr
;; Serialize a binding as an s-expression.
(define (binding->sexp a-binding)
  (cond
    [(binding:constant? a-binding)
     (list 'binding:constant 
           (binding:constant-name a-binding)
           (binding:constant-module-source a-binding)
           ""
           (map permission->string (binding:constant-permissions a-binding)))]
    [(binding:function? a-binding)
     (list 'binding:function
           (binding:function-name a-binding)
           (binding:function-module-source a-binding)
           (binding:function-min-arity a-binding)
           (binding:function-var-arity? a-binding)
           ""
           (map permission->string (binding:function-permissions a-binding))
           (binding:function-cps? a-binding))]
    [(binding:structure? a-binding)
     (list 'binding:structure
           (binding:structure-name a-binding)
           (binding:structure-module-source a-binding)
           (binding:structure-fields a-binding)
           (binding:structure-constructor a-binding)
           (binding:structure-predicate a-binding)
           (binding:structure-accessors a-binding)
           (binding:structure-mutators a-binding))]))




;; sexp->binding: sexp -> binding
;; Thaw out an s-expression back to a binding.
;;
;; FIXME: we need to do this defensively, as the bindings are from the outside
;; world.
(define (sexp->binding an-sexp)
  (case (first an-sexp)
    [(binding:constant)
     (cond [(= (length an-sexp) 4)
            (make-binding:constant (list-ref an-sexp 1)
                                   "something"
                                   (list-ref an-sexp 2)
                                   (map string->permission (list-ref an-sexp 3)))]
           [else            
            (make-binding:constant (list-ref an-sexp 1)
                                   (list-ref an-sexp 2)
                                   (list-ref an-sexp 3)
                                   (map string->permission (list-ref an-sexp 4)))])]
    [(binding:function)
     (make-binding:function (list-ref an-sexp 1)
                            (list-ref an-sexp 2)
                            (list-ref an-sexp 3)
                            (list-ref an-sexp 4)
                            (list-ref an-sexp 5)
                            (map string->permission (list-ref an-sexp 6))
                            (list-ref an-sexp 7))]
    [(binding:structure)
     (cond [(= (length an-sexp) 7)
            (make-binding:structure (list-ref an-sexp 1)
                                    "something"
                                    (list-ref an-sexp 2)
                                    (list-ref an-sexp 3)
                                    (list-ref an-sexp 4)
                                    (list-ref an-sexp 5)
                                    (list-ref an-sexp 6))]
           [else
            (make-binding:structure (list-ref an-sexp 1)
                                    (list-ref an-sexp 2)
                                    (list-ref an-sexp 3)
                                    (list-ref an-sexp 4)
                                    (list-ref an-sexp 5)
                                    (list-ref an-sexp 6)
                                    (list-ref an-sexp 7))])]))


;; localize-binding-to-module: binding module-name -> binding
;; Rename all the javastring references so they're module-scoped.
(define (localize-binding-to-module a-binding a-module-name)
  (cond
    [(binding:constant? a-binding)
     (make-binding:constant 
      (binding:constant-name a-binding)
      (binding:constant-module-source a-binding)
      (format "plt._MODULES[~s].EXPORTS[~s]"
              (symbol->string a-module-name)
              (symbol->string (binding:constant-name a-binding)))
      (binding:constant-permissions a-binding))]
    [(binding:function? a-binding)
     (make-binding:function
      (binding:function-name a-binding)
      (binding:function-module-source a-binding)
      (binding:function-min-arity a-binding)
      (binding:function-var-arity? a-binding)
      (format "plt._MODULES[~s].EXPORTS[~s]"
              (symbol->string a-module-name)
              (symbol->string (binding:function-name a-binding)))
      (binding:function-permissions a-binding)
      (binding:function-cps? a-binding))]
    [(binding:structure? a-binding)
     (make-binding:structure
      (binding:structure-name a-binding)
      (binding:structure-module-source a-binding)
      (binding:structure-fields a-binding)
      (binding:structure-constructor a-binding)
      (binding:structure-predicate a-binding)
      (binding:structure-accessors a-binding)
      (binding:structure-mutators a-binding))]))





(define-struct module-binding (name source bindings))


;; module-name?: any -> boolean
;; A module name is a symbol.
(define (module-name? x)
  (symbol? x))


;; module-path?: any -> boolean
;; A module path is either a symbol or a string.
(define (module-path? x)
  (or (symbol? x)
      (string? x)))



;; module-path=?: module-path module-path -> boolean
;; Returns true if the module paths are the same.
(define (module-path=? p1 p2)
  (cond
    [(and (symbol? p1) (symbol? p2))
     (eq? p1 p2)]
    [(and (string? p1) (string? p2))
     (string=? p1 p2)]
    [else
     #f]))



(define (binding-update-permissions a-binding permissions)
  (cond
    [(binding:constant? a-binding)
     (make-binding:constant (binding:constant-name a-binding)
                            (binding:constant-module-source a-binding)
                            permissions)]
    [(binding:function? a-binding)
     (make-binding:function (binding:function-name a-binding)
                            (binding:function-module-source a-binding)
                            (binding:function-min-arity a-binding)
                            (binding:function-var-arity? a-binding)
                            permissions
                            (binding:function-cps? a-binding))]
    [(binding:structure? a-binding)
     (make-binding:structure (binding:structure-name a-binding)
                             (binding:structure-module-source a-binding)
                             (binding:structure-fields a-binding)
                             (binding:structure-constructor a-binding)
                             (binding:structure-predicate a-binding)
                             (binding:structure-accessors a-binding)
                             (binding:structure-mutators a-binding)
                             permissions)]))


(define (binding-permissions a-binding)
  (cond
    [(binding:constant? a-binding)
     (binding:constant-permissions a-binding)]
    [(binding:function? a-binding)
     (binding:function-permissions a-binding)]
    [(binding:structure? a-binding)
     (binding:structure-permissions a-binding)]))






(provide
 
 [struct-out binding:constant]
 [struct-out binding:function]
 [struct-out binding:structure]
 
 binding?
 binding-id
 binding->sexp
 binding-update-permissions
 binding-permissions
 sexp->binding
 
 localize-binding-to-module
 
 binding-module-source
  
 [struct-out module-binding]
 module-name?
 module-path?
 module-path=?)
