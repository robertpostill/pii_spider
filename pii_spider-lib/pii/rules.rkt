#lang racket/base

(require racket/match
         racket/string
         "../structs.rkt")

(provide email au-phone-number credit-card au-tax-file-number password all-rules)

;; TODO have this maybe with levels of expense for deeper checking i.e. level 1 - regexp level 2 - domain check level 3 - test email
(define (email candidate)
  (define rule-name "Email Address")
  (define simple-email-regex (pregexp "\\S+@\\S+\\.\\S+"))
  (match candidate
    [(pregexp simple-email-regex)
     (examined-data null null rule-name (regexp-match*
                                               simple-email-regex candidate) null #t)]
    [_ (examined-data null null rule-name null null #f)]))

;; it occurs to me that this is going to be a problem 
(define (au-phone-number candidate)
  (define rule-name "AU Phone Number")
  (define local-regex #px"\\b0[1234578]\\d{2}\\s?\\d{3}\\s?\\d{3}\\b")
  ; ideally this would have a \\b in front but that seems to cause a bug
  ;; see https://github.com/racket/racket/issues/4213
  (define international-regex #px"\\+61\\s?[1234578]\\d{2}\\s?\\d{3}\\s?\\d{3}\\b") 
  
  (match candidate
    [(pregexp local-regex)
     (examined-data null null rule-name (regexp-match*
                                               local-regex candidate) null #t)]
    [(pregexp international-regex)
     (examined-data null null rule-name (regexp-match*
                                               international-regex candidate) null #t)]
    [_ (examined-data null null rule-name null null #f)]))

;; Check this handy helper for more CC number formats
;; https://en.wikipedia.org/wiki/Payment_card_number
;; https://stackoverflow.com/questions/9315647/regex-credit-card-number-tests
;; https://www.creditcardinsider.com/learn/anatomy-of-a-credit-card/
;; https://www.paypalobjects.com/en_GB/vhelp/paypalmanager_help/credit_card_numbers.htm
;; TODO handle number fields correctly
;; TODO handle more card issuers
;; TODO validate the card numbers more thoroughly
(define (credit-card candidate)
  (define rule-name "Credit Card")
  (define visa-mc-regex (pregexp "[452]\\d{3}[\\s-]?\\d{4}[\\s-]?\\d{4}[\\s-]?\\d{4}"))
  (define amex-regex (pregexp "3[47]\\d{2}[\\s-]?\\d{6}[\\s-]?\\d{5}"))
  (match candidate
    [(pregexp visa-mc-regex) 
     (examined-data null null rule-name (regexp-match* visa-mc-regex candidate) null #t)]
    [(pregexp amex-regex)
     (examined-data null null rule-name (regexp-match* amex-regex candidate) null #t)]
    [_ (examined-data null null rule-name null null #f)]))

(define (au-tax-file-number candidate)
  (define rule-name "AU Tax File Number")
  (define tfn-regex (pregexp "\\b(\\d{3})\\s?(\\d{3})\\s?(\\d{3})\\b"))
  (match candidate
    [(pregexp tfn-regex) #:when (valid-tfn? candidate)
                         (examined-data null null rule-name (filter valid-tfn? (regexp-match*
                                                                                tfn-regex candidate)) null #t)]
    [_ (examined-data null null rule-name null null #f)]))

;; see https://www.clearwater.com.au/code/tfn for the procedure used to calculate this
;;;  NB there are special TFNs see https://support.yourpayroll.com.au/hc/en-au/articles/200077535-Special-Tax-File-Numbers
(define (valid-tfn? candidate)
  (define tfn-regex (pregexp "(\\d{3})\\s?(\\d{3})\\s?(\\d{3})"))
  (define magic-weights '(1 4 3 7 5 8 6 9 10))
  (define 0-code (char->integer #\0))
  (define result  (match candidate
                    [(pregexp "111\\s?111\\s?111") #t]
                    [(pregexp "333\\s?333\\s?333") #t]
                    [(pregexp "444\\s?444\\s?444") #t]
                    [(pregexp tfn-regex (cons _ (app string-append* s)))
                     (zero? (remainder
                             (for/sum ([c (in-string s)]
                                       [w (in-list magic-weights)])
                               (* (- (char->integer c) 0-code) w))
                             11))]
                    [_ #f]))
  result)

;; TODO have a go at medicare

(define (password candidate)
  (define rule-name "Password")
  (define simple-regex #px"\\b(password[:]?[\\s]+)(\\S*)\\b")
  (match candidate
    [(pregexp simple-regex)
     (examined-data null null rule-name (pluck-passwords
                                                simple-regex candidate) null #t)]
    [_ (examined-data null null rule-name null null #f)]))

(define (pluck-passwords regex candidate)
  (map (lambda (matched-data)
         (regexp-replace (pregexp "password[:]?[\\s]+") matched-data ""))
       (regexp-match* regex candidate)))

(define all-rules (list email password credit-card au-tax-file-number au-phone-number))
