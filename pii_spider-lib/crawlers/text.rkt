#lang racket/base

(require "../structs.rkt"
         "../pii/rules.rkt"
         gregor)

(provide crawl-text)

(define (crawl-text text settings)
  (define started-at (now/moment))
  (define result "")
  (examined-text result null started-at (now/moment)))
