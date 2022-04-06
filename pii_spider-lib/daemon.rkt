#lang racket/base

(require web-server/dispatch
         web-server/servlet-env
         web-server/http/request-structs
         net/url-string
         json
         gregor
         koyo/json
         "logging.rkt")

(provide listen dispatcher 500-responder 404-responder examine)

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

(define (examine request)
  (define original-data (bytes->string/utf-8 (request-post-data/raw request)))
  (log-agent-debug
   (string-append "POSTed data: " original-data))
  (let
      ([req-body (string->jsexpr original-data)]
       [result (make-hash)])
      ;; (crawl-string )
      (hash-set! result 'originalData (hash-ref req-body 'scanData))
      (response/json
       result
       #:code 200
       #:headers (list (header #"Access-Control-Allow-Origin" #"http://localhost:3000"))))
  )

