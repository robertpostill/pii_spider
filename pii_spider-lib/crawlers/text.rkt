#lang racket/base

(require racket/string
         "../structs.rkt"
         (prefix-in rules: "../pii/rules.rkt")
         gregor
         uuid)

(provide crawl-text)

(define (crawl-text text settings)
  (define started-at (now/moment))
  (define rules rules:all-rules)
  (define result (foldl (lambda (rule results)
                          (apply-rule-to-text results rule))
                        (list text null)
                        rules))
  (examined-text (car result) (cadr result) rules started-at (now/moment)))

(define (apply-rule-to-text results rule)
  (let ([result (rule (car results))])
    (if (examined-data-rule-triggered result)
        (list (markup-text (car results) result) (cons result (cadr results)))
        (list (car results) (cadr results)))))

(define (markup-text text rule-result)
  (define matched-data (examined-data-matched-data rule-result))
  (foldl (lambda (matched-item text-to-mark)
           (define generated-id (uuid-string))
           (define data-to-place (string-append "[[privay:" generated-id "][" matched-item "]]"))
           (string-replace text-to-mark matched-item data-to-place #:all? #f))
         text
         matched-data))
