#lang racket/base

(require racket/function
         gregor
         rackunit
         mock
         mock/rackunit
         pii_spider/crawlers/text
         pii_spider/structs)

(provide text-tests)

(define text-tests
  (test-suite
   "text"
   (test-suite
    "crawl-text"
    (test-case "returns an examined-text struct"
      (check-true (examined-text? (crawl-text "test data" (make-hash)))))
    (test-case "returns a moment for the start time"
      (check-true (moment? (examined-text-start-time (crawl-text "test data" (make-hash))))))
    (test-case "returns a moment for the end time"
      (check-true (moment? (examined-text-end-time (crawl-text "test data" (make-hash))))))
    (test-case "returns HTML text for markup"
      (check-true (string? (examined-text-markup (crawl-text "test data" (make-hash))))))
    (test-case "returns a a list of examined strings for the results"
      (check-true (andmap examined-string? (examined-text-results (crawl-text "test data" (make-hash))))))
    (test-case "returns HTML containing the "))))

(module+ test
  (require rackunit/text-ui)
  (run-tests text-tests))
