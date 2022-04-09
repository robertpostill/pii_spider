#lang racket/base

(require "../structs.rkt"
         (prefix-in rules: "../pii/rules.rkt")
         gregor)

(provide crawl-text)

(define (crawl-text text settings)
  (define started-at (now/moment))
  (define rules rules:all-rules)
  (define wrapped-text (string-append "<p>" text "</p>"))
  (define result (foldr (lambda (rule text) (apply-rule-to-text text rule)) wrapped-text rules))
  (examined-text result null started-at (now/moment)))

(define (apply-rule-to-text text rule) text)
;;;  TODO recursively appy the rule to the text
