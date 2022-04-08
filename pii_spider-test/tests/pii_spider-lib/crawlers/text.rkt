#lang racket/base

(require racket/function
         rackunit
         mock
         mock/rackunit
         pii_spider/crawlers/text)

(provide text-tests)

(define text-tests
  (test-suite
   "text"
   (test-suite
    "crawl"
    (test-case "returns #t"
      (check-true (crawl "test data"))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests text-tests))
