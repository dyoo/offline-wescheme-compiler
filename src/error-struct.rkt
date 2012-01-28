#lang planet dyoo/whalesong


(define-struct moby-error (location error-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The parser errors:
;; A lexical token hasn't been closed (e.g. a string literal without closing quote)
(define-struct moby-error-type:unclosed-lexical-token (type opener closer))
;; A lexical token has been seen that we don't know how to lex.
(define-struct moby-error-type:unrecognized-lexical-token (token))
;; A lexical token has been seen that we don't support (e.g. dotted pairs)
(define-struct moby-error-type:unsupported-lexical-token (token))

;; An unsupported expression form has shown up
(define-struct moby-error-type:unsupported-expression-form (expr))

;; e.g. "("
(define-struct moby-error-type:unclosed-parentheses (opener closer))

;; e.g. ")", 
(define-struct moby-error-type:closing-parenthesis-before-opener (closer))

;; If the parentheses are closed by a paren of unexpected shape, we raise
;; unbalanced-parentheses. 
;; e.g. "( ]"
(define-struct moby-error-type:unbalanced-parentheses (opener closer observed other-location))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-struct moby-error-type:syntax-not-applied (keyword example))


(define-struct moby-error-type:duplicate-identifier (id second-location))
(define-struct moby-error-type:expected-identifier (observed))
(define-struct moby-error-type:expected-list-of-identifiers (who observed))
(define-struct moby-error-type:undefined-identifier (id))
(define-struct moby-error-type:structure-identifier-not-expression (id))
(define-struct moby-error-type:provided-name-not-defined (id))
(define-struct moby-error-type:provided-structure-not-structure (id))
(define-struct moby-error-type:unknown-module (path))
(define-struct moby-error-type:redefinition-not-allowed (id))


(define-struct moby-error-type:conditional-missing-question-answer ())
(define-struct moby-error-type:conditional-malformed-clause ())
(define-struct moby-error-type:conditional-clause-too-few-elements ())
(define-struct moby-error-type:conditional-clause-too-many-elements ())
(define-struct moby-error-type:conditional-exhausted ())

(define-struct moby-error-type:branch-value-not-boolean (observed))

(define-struct moby-error-type:if-too-few-elements ())
(define-struct moby-error-type:if-too-many-elements ())

(define-struct moby-error-type:missing-expression-following-quote (quote-stx))  ;; e.g. (list hello ')

(define-struct moby-error-type:quote-too-few-elements ())
(define-struct moby-error-type:quote-too-many-elements ())
(define-struct moby-error-type:quasiquote-too-few-elements ())
(define-struct moby-error-type:quasiquote-too-many-elements ())
(define-struct moby-error-type:unquote-too-few-elements ())
(define-struct moby-error-type:unquote-too-many-elements ())
(define-struct moby-error-type:unquote-splicing-too-few-elements ())
(define-struct moby-error-type:unquote-splicing-too-many-elements ())


(define-struct moby-error-type:begin-body-empty ())

(define-struct moby-error-type:boolean-chain-too-few-elements (id))

(define-struct moby-error-type:lambda-too-few-elements ())
(define-struct moby-error-type:lambda-too-many-elements ())

(define-struct moby-error-type:when-no-body ())
(define-struct moby-error-type:unless-no-body ())

(define-struct moby-error-type:check-expect (expected observed))
(define-struct moby-error-type:check-within (expected observed within))
(define-struct moby-error-type:check-error (expected observed))
(define-struct moby-error-type:check-error-no-error (expected observed))

(define-struct moby-error-type:application-arity (who expected observed))
(define-struct moby-error-type:application-operator-not-a-function (who val))
(define-struct moby-error-type:type-mismatch (who position expected observed))
(define-struct moby-error-type:index-out-of-bounds (minimum maximum observed))



(define-struct moby-error-type:generic-runtime-error (reason))
;; FIXME: the generic-syntactic-error class should die as soon as I fully enumerate
;; the errors.
(define-struct moby-error-type:generic-syntactic-error (reason other-locations))

(define-struct moby-error-type:generic-read-error (message locations))




;; moby-error-type: any -> boolean
;; Produces true if x is a moby-error-type.
(define (moby-error-type? x)
  (or (moby-error-type:unclosed-lexical-token? x)
      (moby-error-type:unrecognized-lexical-token? x)
      (moby-error-type:unsupported-lexical-token? x)
      (moby-error-type:unsupported-expression-form? x)
      (moby-error-type:unclosed-parentheses? x)
      (moby-error-type:unbalanced-parentheses? x)
      (moby-error-type:syntax-not-applied? x)
      (moby-error-type:closing-parenthesis-before-opener? x)
      (moby-error-type:duplicate-identifier? x)
      (moby-error-type:expected-identifier? x)
      (moby-error-type:expected-list-of-identifiers? x)
      (moby-error-type:undefined-identifier? x)
      (moby-error-type:structure-identifier-not-expression? x)
      (moby-error-type:provided-name-not-defined? x)
      (moby-error-type:provided-structure-not-structure? x)
      (moby-error-type:unknown-module? x)
      (moby-error-type:redefinition-not-allowed? x)
      (moby-error-type:conditional-missing-question-answer? x)
      (moby-error-type:conditional-exhausted? x)
      (moby-error-type:conditional-missing-question-answer? x)
      (moby-error-type:conditional-malformed-clause? x)
      (moby-error-type:conditional-clause-too-few-elements? x)
      (moby-error-type:conditional-clause-too-many-elements? x)
      (moby-error-type:branch-value-not-boolean? x)
      (moby-error-type:if-too-few-elements? x)
      (moby-error-type:if-too-many-elements? x)

      (moby-error-type:boolean-chain-too-few-elements? x)
      (moby-error-type:begin-body-empty? x)

      (moby-error-type:lambda-too-many-elements? x)
      (moby-error-type:lambda-too-few-elements? x)

      (moby-error-type:missing-expression-following-quote? x)
      (moby-error-type:quote-too-few-elements? x)
      (moby-error-type:quote-too-many-elements? x)
      (moby-error-type:quasiquote-too-few-elements? x)
      (moby-error-type:quasiquote-too-many-elements? x)
      (moby-error-type:unquote-too-few-elements? x)
      (moby-error-type:unquote-too-many-elements? x)
      (moby-error-type:unquote-splicing-too-few-elements? x)
      (moby-error-type:unquote-splicing-too-many-elements? x)
      
      (moby-error-type:when-no-body? x)
      (moby-error-type:unless-no-body? x)

      (moby-error-type:check-expect? x)
      (moby-error-type:check-within? x)
      (moby-error-type:check-error? x)
      (moby-error-type:check-error-no-error? x)
      
      (moby-error-type:application-arity? x)
      (moby-error-type:application-operator-not-a-function? x)
      (moby-error-type:type-mismatch? x)
      (moby-error-type:index-out-of-bounds? x)
      (moby-error-type:generic-runtime-error? x)
      (moby-error-type:generic-syntactic-error? x)
      (moby-error-type:generic-read-error? x)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct moby-expected:string ())
(define-struct moby-expected:integer ())
(define-struct moby-expected:natural ())
(define-struct moby-expected:rational ())
(define-struct moby-expected:real ())
(define-struct moby-expected:complex ())
(define-struct moby-expected:number ())
(define-struct moby-expected:boolean ())
(define-struct moby-expected:char ())
(define-struct moby-expected:symbol ())
(define-struct moby-expected:list ())
(define-struct moby-expected:listof (thing))
(define-struct moby-expected:vector ())
(define-struct moby-expected:struct ())
(define-struct moby-expected:box ())
(define-struct moby-expected:hash ())
(define-struct moby-expected:function ())
(define-struct moby-expected:something (description))


;; moby-expected?: any -> boolean
;; Produces true if x is an expected value.
(define (moby-expected? x)
  (ormap (lambda (pred?)
           (pred? x))
         (list 
          moby-expected:string?
          moby-expected:integer?
          moby-expected:natural?
          moby-expected:rational?
          moby-expected:real?
          moby-expected:complex?
          moby-expected:number?
          moby-expected:boolean?
          moby-expected:char?
          moby-expected:symbol?
          moby-expected:list?
          moby-expected:vector?
          moby-expected:struct?
          moby-expected:box?
          moby-expected:hash?
          moby-expected:function?
          moby-expected:something?)))
      



(provide
 moby-error-type?
 [struct-out moby-error]
                     
 
 [struct-out moby-error-type:unclosed-lexical-token]
 [struct-out moby-error-type:unrecognized-lexical-token]
 [struct-out moby-error-type:unsupported-lexical-token]
 [struct-out moby-error-type:unsupported-expression-form]
 [struct-out moby-error-type:unclosed-parentheses]
 [struct-out moby-error-type:unbalanced-parentheses]
 [struct-out moby-error-type:closing-parenthesis-before-opener]
 [struct-out moby-error-type:syntax-not-applied]
 [struct-out moby-error-type:duplicate-identifier]
 [struct-out moby-error-type:expected-identifier]
 [struct-out moby-error-type:expected-list-of-identifiers]
 [struct-out moby-error-type:undefined-identifier]
 [struct-out moby-error-type:structure-identifier-not-expression]
 [struct-out moby-error-type:provided-name-not-defined]
 [struct-out moby-error-type:provided-structure-not-structure]
 [struct-out moby-error-type:unknown-module]
 [struct-out moby-error-type:redefinition-not-allowed]
 
 [struct-out moby-error-type:conditional-missing-question-answer]    ;; missing clauses
 [struct-out moby-error-type:conditional-malformed-clause]           ;; a clause which isn't an [question answer]
 [struct-out moby-error-type:conditional-clause-too-few-elements]  ;; a clause without a question or an answer
 [struct-out moby-error-type:conditional-clause-too-many-elements] ;; a clause with too many answer values
 [struct-out moby-error-type:conditional-exhausted]             ;; runtime: no answer was true
 
 [struct-out moby-error-type:branch-value-not-boolean]
 
 [struct-out moby-error-type:if-too-few-elements]   ;; e.g. (if x)
 [struct-out moby-error-type:if-too-many-elements]  ;; (if x y z w)

 [struct-out moby-error-type:begin-body-empty]      ;; e.g. (begin)
 
 [struct-out moby-error-type:boolean-chain-too-few-elements]
 [struct-out moby-error-type:lambda-too-many-elements]
 [struct-out moby-error-type:lambda-too-few-elements]

 [struct-out moby-error-type:missing-expression-following-quote]
 [struct-out moby-error-type:quote-too-few-elements]
 [struct-out moby-error-type:quote-too-many-elements]
 [struct-out moby-error-type:quasiquote-too-few-elements]
 [struct-out moby-error-type:quasiquote-too-many-elements]
 [struct-out moby-error-type:unquote-too-few-elements]
 [struct-out moby-error-type:unquote-too-many-elements]
 [struct-out moby-error-type:unquote-splicing-too-few-elements]
 [struct-out moby-error-type:unquote-splicing-too-many-elements]
 
 [struct-out moby-error-type:when-no-body]
 [struct-out moby-error-type:unless-no-body]
  
 [struct-out moby-error-type:check-expect]
 
 [struct-out moby-error-type:check-within]
 
 [struct-out moby-error-type:check-error]  ;; the expected string of the error message and the observed string
 [struct-out moby-error-type:check-error-no-error]
 
 
 [struct-out moby-error-type:application-arity]
 [struct-out moby-error-type:application-operator-not-a-function    ;; who is the operator
                                                                    ;; what value 
                                                                    ;; did the operator produce?
                                                              ]
 [struct-out moby-error-type:type-mismatch]
 [struct-out moby-error-type:index-out-of-bounds]

 [struct-out moby-error-type:generic-runtime-error]
 [struct-out moby-error-type:generic-syntactic-error]
 

 [struct-out moby-error-type:generic-read-error]
  
 
 moby-expected?
 [struct-out moby-expected:string]
 [struct-out moby-expected:integer]
 [struct-out moby-expected:natural]
 [struct-out moby-expected:rational]
 [struct-out moby-expected:real]
 [struct-out moby-expected:complex]
 [struct-out moby-expected:number]
 [struct-out moby-expected:boolean] 
 [struct-out moby-expected:char]
 [struct-out moby-expected:symbol]
 [struct-out moby-expected:list]
 [struct-out moby-expected:listof]
 [struct-out moby-expected:vector]
 [struct-out moby-expected:struct]
 [struct-out moby-expected:box]
 [struct-out moby-expected:hash]
 [struct-out moby-expected:function]
 [struct-out moby-expected:something])