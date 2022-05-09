#lang racket/base

(require racket/struct)

(provide (struct-out examined-row) (struct-out examined-table) (struct-out ignore)
         (struct-out examined-text) (struct-out examined-data)
         (struct-out exn:fail:pii-spider) (struct-out exn:fail:pii-spider:db-connection))

(struct examined-row (id results)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'examined-row)
      (lambda (obj) (list (examined-row-id obj) (examined-row-results obj)))))]) 

(struct examined-table (name start-time end-time row-count results ignored)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'examined-table)
      (lambda (obj) (list (examined-table-name obj)
                          (examined-table-row-count obj)
                          (examined-table-end-time obj)
                          (examined-table-ignored obj)))))])

(struct ignore (tables columns rows)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'ignores)
      (lambda (obj) (list (ignore-tables obj)
                          (ignore-columns obj)
                          (ignore-rows obj)))))])

(struct examined-text (markup triggered-rules rules-applied start-time end-time)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'examined-text)
      (lambda (obj) (list (examined-text-markup obj)
                          (examined-text-triggered-rules obj)
                          (examined-text-end-time obj)
                          (examined-text-start-time obj)))))])

(struct examined-data (id reference rule matched-data rule-triggered)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'examined-data)
      (lambda (obj) (list (examined-data-id obj)
                          (examined-data-rule obj)
                          (examined-data-matched-data obj)
                          (examined-data-rule-triggered obj)))))])


(struct exn:fail:pii-spider exn:fail ())
(struct exn:fail:pii-spider:db-connection exn:fail:pii-spider ())
