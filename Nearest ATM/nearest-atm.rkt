#lang racket
(require "common/uw-api.rkt")
(require racket/undefined)
(require racket/format)
(provide atm-finder)

;; (nearest-atm building atm) finds the location of the closest ATM to the
;; building you are located at
;; atm-finder: Str Str -> Str
;; uses "/buildings/list" and "/poi/atms"

(define (cat-fn cat lst)
  (cond
    [(string=? (first (first lst)) cat) (second (first lst))]
    [else (cat-fn cat (rest lst))]))

(define (building-finder name lst)
  (cond
    [(empty? lst) empty]
    [(or (member name (cat-fn "alternate_names" (first lst)))
      (string=? name (cat-fn "building_name" (first lst)))
      (string=? name (cat-fn "building_code" (first lst)))) (first lst)]
    [else (building-finder name (rest lst))]))

(define (atm-finder name lst)
  (cond
    [(empty? lst) empty]
    [(string=? name (cat-fn "name" (first lst))) (cons (first lst) (atm-finder name (rest lst)))]
    [else (atm-finder name (rest lst))]))

;;(atm-finder "bla" (uw-api "/poi/atms"))
;;(building-finder "Mackenzie King" (uw-api "/buildings/list"))

(define (distance N1 E1 N2 E2)
  (local
    [(define R 6371000);earth radius
     (define phi1 (* (/ pi 180) N1))
     (define phi2 (* (/ pi 180) N2))
     (define d-phi(* (/ pi 180) (- N2 N1)))
     (define d-lam (* (/ pi 180) (- E2 E1)))

     (define a (+ (* (sin (/ d-phi 2)) (sin (/ d-phi 2)))
                  (* (cos phi1) (cos phi2) (sin (/ d-lam 2)) (sin (/ d-lam 2))))) ;;haversine

     (define c (* 2 (atan (sqrt a) (sqrt (- 1 a)))))]
    (/ (round (/ (* R c) 100)) 10)))

(define (my-remainder divident divisor)
  (cond
    [(< divident divisor) divident]
    [else (my-remainder (- divident divisor) divisor)]))

(define (bearing N1 E1 N2 E2)
  (local
    [(define phi1 (* (/ pi 180) N1))
     (define phi2 (* (/ pi 180) N2))
     (define lam1 (* (/ pi 180) E1))
     (define lam2 (* (/ pi 180) E2))
     (define y (* (sin (- lam2 lam1)) (cos phi2)))
     (define x (- (* (cos phi1) (sin phi2)) (* (sin phi1) (cos phi2) (cos (- lam2 lam1)))))
     (define brng (* (/ 180 pi) (atan y x)))]
    (my-remainder (+ brng 360) 360)))

(define (direction n)
  (cond
    [(or (and (<= 348.75 n) (< n 360))(and (<= 0 n) (< n 11.25))) "N"]
    [(and (<= 11.25 n) (< n 33.75)) "NNE"]
    [(and (<= 33.75 n) (< n 56.25)) "NE"]
    [(and (<= 56.25 n) (< n 78.75)) "ENE"]
    [(and (<= 78.75 n) (< n 101.25)) "E"]
    [(and (<= 101.25 n) (< n 123.75)) "ESE"]
    [(and (<= 123.75 n) (< n 146.25)) "SE"]
    [(and (<= 146.25 n) (< n 168.75)) "SSE"]
    [(and (<= 168.75 n) (< n 191.25)) "S"]
    [(and (<= 191.25 n) (< n 213.75)) "SSW"]
    [(and (<= 213.75 n) (< n 236.25)) "SW"]
    [(and (<= 236.25 n) (< n 258.75)) "WSW"]
    [(and (<= 258.75 n) (< n 281.25)) "W"]
    [(and (<= 281.25 n) (< n 303.75)) "WNW"]
    [(and (<= 303.75 n) (< n 326.25)) "NW"]
    [(and (<= 326.25 n) (< n 348.75)) "NNW"]))

;;(distance 50.0359 -.54253 58.3838 -.30412)
;;(bearing 50.0359 -.54253 58.3838 -.30412)
;;(direction (bearing 50.0359 -.54253 58.3838 -.30412))

(define (closest-atm building atm-lst d atm)
  (cond
    [(empty? atm-lst) atm]
    [(empty? atm) (closest-atm building atm-lst (distance (cat-fn "latitude" building) (cat-fn "longitude" building)
                                   (cat-fn "latitude" (first atm-lst)) (cat-fn "longitude" (first atm-lst))) (first atm-lst))]
    [(<(distance (cat-fn "latitude" building) (cat-fn "longitude" building)
                                   (cat-fn "latitude" (first atm-lst)) (cat-fn "longitude" (first atm-lst))) d)
     (closest-atm building (rest atm-lst) (distance (cat-fn "latitude" building) (cat-fn "longitude" building)
                                   (cat-fn "latitude" (first atm-lst)) (cat-fn "longitude" (first atm-lst))) (first atm-lst))]
    [else (closest-atm building (rest atm-lst) d atm)]))

(define (nearest-atm b-name a-name)
  (closest-atm (building-finder b-name (uw-api "/buildings/list"))
               (atm-finder a-name (uw-api "/poi/atms")) 0 empty))


(define (nearest-atm-finder b-name a-name)
  (cond
    [(and (empty? (building-finder b-name (uw-api "/buildings/list"))) (empty? (atm-finder a-name (uw-api "/poi/atms"))))
     "Can't find your specified location and ATM."]
    [(empty? (building-finder b-name (uw-api "/buildings/list"))) "Can't find your location."]
    [(empty? (atm-finder a-name (uw-api "/poi/atms"))) "Can't find your ATM."]
    [else (string-append "Nearest " a-name " ATM is located in " (cat-fn "note" (nearest-atm b-name a-name)) " "
                         (~v (distance (cat-fn "latitude" (building-finder b-name (uw-api "/buildings/list"))) (cat-fn "longitude" (building-finder b-name (uw-api "/buildings/list")))
                                   (cat-fn "latitude" (nearest-atm b-name a-name)) (cat-fn "longitude" (nearest-atm b-name a-name))))
                         " km " (direction (bearing (cat-fn "latitude" (building-finder b-name (uw-api "/buildings/list"))) (cat-fn "longitude" (building-finder b-name (uw-api "/buildings/list")))
                                   (cat-fn "latitude" (nearest-atm b-name a-name)) (cat-fn "longitude" (nearest-atm b-name a-name)))) ".")]))

(nearest-atm-finder "MKV" "TD")

