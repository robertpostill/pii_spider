#lang racket/base

(require rackunit
         uuid
         pii_spider/pii/util)

(provide util-tests)

(define util-tests
  (test-suite
   "util"
   (test-suite
    "add-uuids"
    (test-case "returns a list"
      (check-true (list? (add-uuids (list "test")))))
    (test-case "the first element of each result is a uuid"
      (check-true (uuid-string? (car (car (add-uuids (list "test")))))))
    (test-case "the second element of each result is the match data"
      (check-equal? (cadr (car (add-uuids (list "test")))) "test")))))

(module+ test
  (require rackunit/text-ui)
  (run-tests util-tests))
