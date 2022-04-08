#lang racket/base

(require racket/function
         racket/port
         racket/logging
         racket/promise
         json
         net/url
         pii_spider/daemon
         rackunit
         mock
         mock/rackunit
         web-server/http/request-structs
         web-server/http/response-structs
         pii_spider/structs)

(provide daemon-tests)

(define daemon-tests
  (test-suite
   "daemon"
   (test-suite
    "listen"
    ;; TODO add some integration tests to check the 404/500 handling
    (test-case "spins up the servlet engine with the correct args"
      (define servlet-mock (mock #:behavior (const (void))))
      (listen #:engine servlet-mock)
      (check-mock-called-with? servlet-mock (arguments dispatcher
                                                       #:servlet-regexp #rx""
                                                       #:launch-browser? #f
                                                       #:servlet-responder 500-responder
                                                       #:file-not-found-responder 404-responder
                                                       #:listen-ip #f
                                                       #:port 8080
                                                       #:log-file "requests.log"))))
   (test-suite
    "examine"
    (test-case "examine returns a 200 for a successful response"
      (define mock-request (make-request
                            #"POST"                                       ; method
                            (string->url "http://localhost:8080/examine") ; URI
                            null                                          ; headers
                            (delay (lambda () null))                      ; body
                            #"{\"scanData\": \"some test data\"}"         ; POST data
                            "127.0.0.1"                                   ; host IP
                            80                                            ; host PORT
                            "127.0.0.1"))                                 ; client IP
      (define result (examine mock-request))
      (check-equal? (response-code result) 200))
    (test-case "examine returns JSON for a successful response"
      (define mock-request (make-request
                            #"POST"                                       ; method
                            (string->url "http://localhost:8080/examine") ; URI
                            null                                          ; headers
                            (delay (lambda () null))                      ; body
                            #"{\"scanData\": \"some test data\"}"         ; POST data
                            "127.0.0.1"                                   ; host IP
                            80                                            ; host PORT
                            "127.0.0.1"))                                 ; client IP
      (define result (examine mock-request))
      (check-equal? (call-with-output-bytes (response-output result))
                    (jsexpr->bytes #hash((data . "blah") (originalData . "some test data")))))
    (test-case "examine calls the crawler with the data to be examined"
      (define mock-crawler (mock #:behavior (const (void))))
      (define mock-request (make-request
                            #"POST"                                       ; method
                            (string->url "http://localhost:8080/examine") ; URI
                            null                                          ; headers
                            (delay (lambda () null))                      ; body
                            #"{\"scanData\": \"some test data\"}"         ; POST data
                            "127.0.0.1"                                   ; host IP
                            80                                            ; host PORT
                            "127.0.0.1"))                                 ; client IP
      (define result (examine mock-request #:crawler mock-crawler))
      (check-mock-called-with? mock-crawler (arguments "some test data")))
    (test-case "examine returns a 400 code for malformed JSON"
      (define mock-request (make-request
                            #"POST"                                       ; method
                            (string->url "http://localhost:8080/examine") ; URI
                            null                                          ; headers
                            (delay (lambda () null))                      ; body
                            #"{\"badKey\": \"does not matter\"}"          ; POST data
                            "127.0.0.1"                                   ; host IP
                            80                                            ; host PORT
                            "127.0.0.1"))                                 ; client IP
      (define result (examine mock-request))
      (check-equal? (response-code result) 400))
    (test-case "examine returns a 400 code when the data isn't JSON"
      (define mock-request (make-request
                            #"POST"                                       ; method
                            (string->url "http://localhost:8080/examine") ; URI
                            null                                          ; headers
                            (delay (lambda () null))                      ; body
                            #"not even JSON"                              ; POST data
                            "127.0.0.1"                                   ; host IP
                            80                                            ; host PORT
                            "127.0.0.1"))                                 ; client IP
      (define result (examine mock-request))
      (check-equal? (response-code result) 400)))
   (test-suite
    "404-responder"
    (test-case "404-responder returns a 404"
      (define mock-request (mock #:behavior (const (void))))
      (define result (404-responder mock-request))
      (check-equal? (response-code result) 404))
    (test-case "404-responder returns JSON"
      (define mock-request (mock #:behavior (const (void))))
      (define result (404-responder mock-request))
      (check-equal? (call-with-output-bytes (response-output result))
                    (jsexpr->bytes #hash((notFound . #t))))))
   (test-suite
    "500-responder"
    (test-case "500-responder returns a status code of 500"
      (define test-url (string->url "http://localhost"))
      (define ex (exn:fail:pii-spider "test error" (current-continuation-marks)))
      (define test-logger (make-logger #f (current-logger) 'none #f))
      (define result (let ([my-log (open-output-nowhere)])
                       (with-logging-to-port
                         my-log
                         (lambda ()
                           (current-logger test-logger)
                           (500-responder test-url ex))
                         'error
                         #:logger test-logger)))
      (check-equal? (response-code result) 500))
    (test-case "500-responder returns JSON for a successful response"
      (define test-url (string->url "http://localhost"))
      (define ex (exn:fail:pii-spider "test error" (current-continuation-marks)))
      (define test-logger (make-logger #f (current-logger) 'none #f))
      (define result (let ([my-log (open-output-nowhere)])
                       (with-logging-to-port
                         my-log
                         (lambda ()
                           (current-logger test-logger)
                           (500-responder test-url ex))
                         'error
                         #:logger test-logger)))
      (check-equal? (call-with-output-bytes (response-output result))
                    (jsexpr->bytes #hash((hasError . #t))))))
   (test-suite
    "400-response"
    (test-case "returns a 400"
      (define test-original-data "test bad data")
      (define test-message "this was bad test data")
      (define result (400-response test-original-data test-message))
      (check-equal? (response-code result) 400))
    (test-case "returns the reason for failure"
      (define test-original-data "test bad data")
      (define test-message "this was bad test data")
      (define result (400-response test-original-data test-message))
      (check-equal? (hash-ref (bytes->jsexpr (call-with-output-bytes (response-output result))) 'message)
                    test-message))
    (test-case "returns the data that triggered the issue"
      (define test-original-data "test bad data")
      (define test-message "this was bad test data")
      (define result (400-response test-original-data test-message))
      (check-equal? (hash-ref (bytes->jsexpr (call-with-output-bytes (response-output result))) 'originalData)
                    test-original-data)))
   (test-suite
    "valid-json?"
    (test-case "it does not throw an exception for invalid json"
      (check-not-exn (lambda () (valid-json? "no JSON here"))))
    (test-case "it returns f for invalid JSON"
      (check-false (valid-json? "not JSON, sorry")))
    (test-case "it returns t for valid JSON"
      (check-true (valid-json? "{\"test\": \"data\"}"))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests daemon-tests))
