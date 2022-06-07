#lang racket/base

(require uuid)

(provide add-uuids)

(define (add-uuids matches)
  (map (lambda (match)
         (list (uuid-string) match)) matches))
