#lang racket/base

(require racket/function
         racket/list
         gregor
         rackunit
         mock
         mock/rackunit
         uuid
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
    (test-case "returns a list of examined strings for the results"
      (check-true 
       (andmap examined-data? (examined-text-triggered-rules
                               (crawl-text "test data robert@test.com" (make-hash))))))
    (test-case "returns a list of 1 triggered rule when the data holds 2 matches"
      (define result (examined-text-triggered-rules
                      (crawl-text "test data robert@test.com roberta@test.com" (make-hash))))
      (check-equal? (length result) 1))
    (test-case "returns the matched emails"
      (define expected-result '("robert@test.com" "roberta@test.com"))
      (define result (flatten
                      (map examined-data-matched-data
                           (examined-text-triggered-rules
                            (crawl-text "test data robert@test.com roberta@test.com"
                                        (make-hash))))))
      (check-equal? result expected-result))
    (test-case "returns text containing the results of the scan"
      (define expected-result "some text")
      (check-equal? (examined-text-markup (crawl-text "some text" (make-hash))) expected-result))
    (test-case "returns text containing intresting parts of the scan in a wrapped link"
      (define expected-result (pregexp "\\[\\[privay:\\S*\\]\\[robert@test\\.com\\]\\]"))
      (check-true
       (regexp-match?
        expected-result
        (examined-text-markup (crawl-text "some text for robert@test.com" (make-hash)))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests text-tests))
