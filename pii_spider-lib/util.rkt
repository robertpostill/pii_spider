#lang racket/base
(require racket/port
         json
         "structs.rkt")

(provide valid-json? jsexpr-from-rules-structs)

(define (valid-json? data)
  (with-handlers ([exn:fail:read? (lambda (e) #f)])
    (with-input-from-string data (lambda () (read-json) #t))))

(define (jsexpr-from-rules-structs rule-results)
  (foldl (lambda (rule-result result-hash)
           (hash-set result-hash (string->symbol (examined-data-rule rule-result)) (hash-of-results (examined-data-matched-data rule-result))))
         (make-immutable-hash) rule-results))

(define (hash-of-results results)
  (foldl (lambda (result result-hash)
           (hash-set result-hash (string->symbol (car result)) (cadr result)))
         (make-immutable-hash) results))
