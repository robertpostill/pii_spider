#lang racket/base

(require json
         rackunit
         uuid
         pii_spider/structs
         pii_spider/util)

(provide util-tests)

(define util-tests
  (test-suite
   "util"
   (test-suite
    "valid-json?"
    (test-case "returns #t for valid json"
      (valid-json? "{\"I\" : \"am valid json\"}"))
    (test-case "returns #f for invalid json"
      (valid-json? "I am not JSON")))
   (test-suite
    "jsexpr-from-rules-structs"
    (test-case "returns a jsexpr"
      (define test-examined-data (list (examined-data null "test rule" (list (list
                                                                              (uuid-string) "robert@test.com"
                                                                              (uuid-string) "rob@test.com")) #t)))
      (check-true (jsexpr? (jsexpr-from-rules-structs test-examined-data)))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests util-tests))
