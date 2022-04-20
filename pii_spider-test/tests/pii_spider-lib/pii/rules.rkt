#lang racket/base

(require rackunit
         pii_spider/pii/rules
         pii_spider/structs)

(provide rules-tests)

(define rules-tests
  (test-suite
   "rules"
   (test-suite
    "email"
    (test-case "returns an examined-data struct"
      (check-true (examined-data? (email "test"))))
    (test-case "returns the rule name"
      (check-equal? (examined-data-rule (email "test")) "Email Address"))
    (test-case "returns #t for an email address"
      (check-true (examined-data-rule-triggered (email "robert@test.com"))))
    (test-case "returns the matched data for an email address"
      (check-equal? (examined-data-matched-data (email "robert@test.com")) "robert@test.com"))
    (test-case "returns #t for an email address inside a string"
      (check-true (examined-data-rule-triggered
                   (email "the string is about robert@test.com being tested"))))
    (test-case "returns #f when not an email address"
      (check-false (examined-data-rule-triggered (email "test"))))
    (test-case "returns #f when not a string"
      (check-false (examined-data-rule-triggered (email 1)))))
   
   (test-suite
    "au-phone-number"
    (test-case "returns an examined-data struct"
      (check-true (examined-data? (au-phone-number "test"))))
    (test-case "returns the rule name"
      (check-equal? (examined-data-rule (au-phone-number "test")) "AU Phone Number"))
    (test-case "returns the matched data for an email address"
      (check-equal? (examined-data-matched-data (au-phone-number "0412345678")) "0412345678"))
    (test-case "returns #t for an AU phone number"
      (check-true (examined-data-rule-triggered (au-phone-number "0412345678"))))
    (test-case "returns #t for an AU phone number with country prefix"
      (check-true (examined-data-rule-triggered (au-phone-number "+61412345678"))))
    (test-case "returns #t for an AU phone number with spaces"
      (check-true (examined-data-rule-triggered (au-phone-number "0412 345 678")))
      (check-true (examined-data-rule-triggered (au-phone-number "0412 345678"))))
    (test-case "returns #t for an AU phone number inside a larger string"
      (check-true (examined-data-rule-triggered (au-phone-number "we should ring 0412 345 678")))
      (check-true (examined-data-rule-triggered
                   (au-phone-number "0412 345678 is a nice phone number")))) 
    (test-case "returns #f when not an AU phone number"
      (check-false (examined-data-rule-triggered (au-phone-number "test"))))
    (test-case "returns #f when not a string"
      (check-false (examined-data-rule-triggered (au-phone-number 1)))))
   
   (test-suite
    "credit-card"
    (test-case "returns an examined-data struct"
      (check-true (examined-data? (credit-card "test"))))
    (test-case "returns the matched data for a credit card"
      (check-equal? (examined-data-matched-data
                     (credit-card "4111111111111111")) "4111111111111111"))
    (test-case "returns the rule name"
      (check-equal? (examined-data-rule (credit-card "test")) "Credit Card"))
    (test-case "returns #t for a valid visa card number"
      (check-true (examined-data-rule-triggered (credit-card "4111111111111111"))))
    (test-case "returns #t for a valid visa card number with spaces"
      (check-true (examined-data-rule-triggered (credit-card "4111 1111 1111 1111"))))
    (test-case "returns #t for a valid visa card number with spaces inside a string"
      (check-true (examined-data-rule-triggered
                   (credit-card "a credit card called 4111 1111 1111 1111 is hidden here"))))
    (test-case "returns #t for a valid visa card number with hyphens"
      (check-true (examined-data-rule-triggered (credit-card "4111-1111-1111-1111"))))
    (test-case "returns #t for a valid visa card number with spaces and hypens"
      (check-true (examined-data-rule-triggered (credit-card "4111 1111-1111 1111"))))
    (test-case "returns #t for a valid amex card number"
      (check-true (examined-data-rule-triggered (credit-card "371238839571772"))))
    (test-case "returns #t for a valid amex card number with spaces"
      (check-true (examined-data-rule-triggered (credit-card "3712 388395 71772"))))
    (test-case "returns #f when not a credit card number"
      (check-false (examined-data-rule-triggered (credit-card "test")))))
   
   (test-suite
    "au-tax-file-number"
    (test-case "returns an examined-data struct"
      (check-true (examined-data? (au-tax-file-number "test"))))
    (test-case "returns the matched data for a TFN"
      (check-equal? (examined-data-matched-data
                     (au-tax-file-number "123456782")) "123456782"))
    (test-case "returns the rule name"
      (check-equal? (examined-data-rule (au-tax-file-number "test")) "AU Tax File Number"))
    (test-case "returns #f when not a AU tax file number"
      (check-false (examined-data-rule-triggered (au-tax-file-number "test"))))
    (test-case "returns #t for a valid AU TFN"
      (check-true (examined-data-rule-triggered (au-tax-file-number "123456782"))))
    (test-case "returns #t for a valid AU TFN with spaces"
      (check-true (examined-data-rule-triggered (au-tax-file-number "123 456 782"))))
    (test-case "returns #f for an invalid AU TFN"
      (check-false (examined-data-rule-triggered (au-tax-file-number "123456789")))))
   
   (test-suite
    "password"
    (test-case "returns an examined-data struct"
      (check-true (examined-data? (password "test"))))
    (test-case "returns the matched data for a TFN"
      (check-equal? (examined-data-matched-data
                     (password "password: password123")) "password123"))
    (test-case "returns the rule name"
      (check-equal? (examined-data-rule (password "test")) "Password"))
    (test-case "returns #f when not a likely password"
      (check-false (examined-data-rule-triggered (password "test"))))
    (test-case "returns #t for a likely password preceeded by the word password"
      (check-true (examined-data-rule-triggered (password "password: passw0rd")))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests rules-tests))
