#lang racket

;; SEASHELL_READONLY

; A simple interface for the uWaterloo API
; See: https://github.com/uWaterloo/api-documentation
; for more information

; Developed by Dave Tompkins [dtompkins AT uwaterloo.ca]
; for cs136 assignments

; version 1.2 [January 2017]

(provide uw-api)

;; A Key is a Str
;; A Val is a (anyof Num Str)

; an APIResult is one of:
; * (list Key Val)
; * (list Key APIResult)               
; * (listof APIResult)

; (uw-api s) will make in inquiry to the UWaterloo API.
;   The format of the results will depend on the API selected,
;   but will typically be a list of lists, where each sublist
;   is a Key/Value pair.

; uw-api: Str -> (anyof APIResult #f)
; requires: you can connect to UW (have online access)

; EXAMPLES:

; (uw-api "/weather/current")
; (uw-api "/events/holidays")
; (uw-api "/courses/CS/136")
; (uw-api "/terms/1171/CS/136/schedule")
; (uw-api "/foodservices/products/2189")
; (uw-api "/foodservices/2017/2/menu")
; (uw-api "/directory/dtompkins")
; (uw-api "/parking/watpark")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require net/url)
(require json)

; (see documentation above)
(define (uw-api s)
  
  ; (safe-hash-ref h k f) produces value for key k in hash h,
  ;   or produces f if h is invalid or k does not exist in h
  (define (safe-hash-ref h k f)
    (cond [(and (hash? h) (hash-has-key? h k))
           (hash-ref h k)]
          [else f]))
  
  ; (hash->strlist h) recursively converts hash h to a list of lists,
  ;   where each sublist is (list Key Val)
  (define (hash->strlist h)
    (cond [(list? h) (map hash->strlist h)]
          [(symbol? h) (symbol->string h)]
          [(not (hash? h)) h]
          [else 
           (hash-map h (lambda (k v) 
                         (list (hash->strlist k) (hash->strlist v))))]))
  (define api-base "https://api.uwaterloo.ca/v2")
  (define api-lang "json")  
  (define api-key "123afda14d0a233ecb585591a95e0339")
  (define url (string->url (string-append 
                            api-base s "." api-lang "?key=" api-key)))
  (define json (read-json (get-pure-port url)))
  (define result (hash->strlist (safe-hash-ref json 'data (make-hash))))
  
  (cond [(empty? result) #f]
        [else result]))
