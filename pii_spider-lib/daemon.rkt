#lang racket/base

(require racket/match
         racket/port
         web-server/dispatch
         web-server/servlet-env
         web-server/http/request-structs
         net/url-string
         json
         gregor
         koyo/json
         "logging.rkt"
         "crawlers/text.rkt")

(provide listen dispatcher 500-responder 404-responder 400-response examine valid-json?)

(define (listen #:engine [serve/servlet serve/servlet] #:log-file [log-file "requests.log"])
  (serve/servlet
   dispatcher
   #:servlet-responder 500-responder
   #:port 8080
   #:listen-ip #f
   #:launch-browser? #f
   #:servlet-regexp #rx""
   #:file-not-found-responder 404-responder
   #:log-file log-file))

(define-values (dispatcher dispatch-url)
  (dispatch-rules
   [("examine") #:method "post" examine]))

(define (500-responder url ex)
  (log-error (format  "[ ~a]  ~a  --->  ~a"
                      (datetime->iso8601 (now/utc))
                      (url->string url)
                      (exn-message ex)))
  (response/json
   #hash((hasError . #t))
   #:code 500
   #:headers (list (header #"Access-Control-Allow-Origin" #"http://localhost:3000"))))

(define (404-responder req)
  (response/json
   #hash((notFound . #t))
   #:code 404
   #:headers (list (header #"Access-Control-Allow-Origin" #"http://localhost:3000"))))

(define (400-response original-data error-message)
  (define result (make-hash))
  (hash-set! result 'originalData original-data)
  (hash-set! result 'message error-message)
  (response/json
   result
   #:code 400
   #:headers (list (header #"Access-Control-Allow-Origin" #"http://localhost:3000"))))

(define (examine request #:crawler [crawl-text crawl-text])
  (define original-data (bytes->string/utf-8 (request-post-data/raw request)))
  (log-agent-debug
   (string-append "POSTed data: " original-data))
  (match (valid-json? original-data)
    [#t #:when (not (hash-has-key? (string->jsexpr original-data) 'scanData)) (400-response original-data "The scanData key can't be read from this JSON")]
    [#t (let
            ([req-body (string->jsexpr original-data)]
             [result (make-hash)])
            (crawl-text (hash-ref req-body 'scanData) (make-hash))
          (hash-set! result 'originalData (hash-ref req-body 'scanData))
          (hash-set! result 'data "blah")
          
          (response/json
           result
           #:code 200
           #:headers (list (header #"Access-Control-Allow-Origin" #"http://localhost:3000"))))]
    [#f (400-response original-data "This data does not appear to be valid JSON")]))

(define (valid-json? data)
  (with-handlers ([exn:fail:read? (lambda (e) #f)])
    (with-input-from-string data (lambda () (read-json) #t))))

