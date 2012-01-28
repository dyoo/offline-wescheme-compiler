#lang racket/base

;; A labeled translation consists of a label, and a translation
(define-struct labeled-translation (label  ;; number
                                    translation ;; string
                                    ))

(provide [struct-out labeled-translation])
