;;;;
;;;;   Copyright (c) Ludger Solbach. All rights reserved.
;;;;
;;;;   The use and distribution terms for this software are covered by the
;;;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;;;   which can be found in the file license.txt at the root of this distribution.
;;;;   By using this software in any fashion, you are agreeing to be bound by
;;;;   the terms of this license.
;;;;
;;;;   You must not remove this notice, or any other, from this software.
;;;;

(ns org.soulspace.astronomy.coordinates
  "Coordinates functions and abstractions."
  (:require [org.soulspace.math.core :as m]
            [org.soulspace.astronomy.time :as time]))

;;;
;;; Coordinates functions and abstractions
;;;

(def ^:const pi-ninetieth "Defines Pi/90 for speed." (/ m/PI 90)) ; 2 degrees

(defn angular-distance
  "Calculates the angular distance between the coordinates (given in rad)."
  (^double [[^double ra1 ^double dec1] [^double ra2 ^double dec2]]
   (angular-distance ra1 dec1 ra2 dec2))
  (^double [^double ra1 ^double dec1 ^double ra2 ^double dec2]
   (let [delta-ra (- ra1 ra2)
         delta-dec (- dec1 dec2)]
     (if (or (< (abs (- (abs dec1) m/HALF-PI)) pi-ninetieth) (< (abs (- (abs dec2) m/HALF-PI)) pi-ninetieth))
       (m/ahav (+ (m/hav delta-dec) (* (m/cos dec1) (m/cos dec2) (m/hav delta-ra)))) ; use haversine if declinations are near the poles
       (m/acos (+ (* (m/sin dec1) (m/sin dec2)) (* (m/cos dec1) (m/cos dec2) (m/cos delta-ra))))))))

(defn zenit-distance-by-altitude
  "Calculates the zenit distance by altitude (given in rad)."
  ^double [^double altitude]
  (- m/HALF-PI (min (abs altitude) m/HALF-PI)))

(defn altitude-by-zenit-distance
  "Calculates the altitude by zenit distance (given in rad)."
  ^double [^double zenit-distance]
  (- m/HALF-PI (min (abs zenit-distance))))

(defn hour-angle
  "Calculates the hour angle of the right ascension at the given instant."
  (^double [instant ^double ra]
   (- (time/mean-siderial-time-greenwich instant) ra)))


;;;
;;; Conversions between spherical and cartesian coordinates
;;;
;;; References:
;;; Montenbruck, Oliver; Grundlagen der Ephemeridenrechnung; 7. Aufl.; Spektrum Akademischer Verlag
;;;

(defn- calc-beta
  [z rho]
  (if (== rho 0)
    (cond
      (< z 0) (m/deg-to-rad -90)
      (== z 0) 0
      (> z 0) (m/deg-to-rad 90))
    (m/atan (/ z rho))))

(defn- calc-lambda
  [x y phi]
  (cond
    (and (== x 0) (== y 0)) 0.0
    (and (>= x 0) (>= y 0)) phi
    (and (>= x 0) (< y 0)) (+ (m/deg-to-rad 360) phi)
    (< x 0) (- (m/deg-to-rad 180) phi)))

(defn cartesian-to-spherical
  "Converts cartesian to spherical coordinates."
  ([cartesian-coords]
   (let [[x y z] cartesian-coords]
     (cartesian-to-spherical x y z)))
  ([x y z]
   (let [r (m/sqrt (+ (m/sqr x) (m/sqr y) (m/sqr z)))
         rho (m/sqrt (+ (m/sqr x) (m/sqr y)))
         beta (calc-beta z rho)
         phi (* 2 (m/atan (/ y (+ (abs x) rho))))
         lambda (calc-lambda x y phi)]
     [r beta lambda])))

(defn spherical-to-cartesian
  "Converts spherical to cartesian coordinates."
  ([spherical-coords]
   (let [[r beta lambda] spherical-coords]
     (spherical-to-cartesian r beta lambda)))
  ([r beta lambda]
   [(* r (m/cos beta) (m/cos lambda))
    (* r (m/cos beta) (m/sin lambda))
    (* r (m/sin beta))]))


;;;
;;; implementations of spherical projections, no ellipsoid projections implemented yet
;;;
;;; References:
;;; Snyder, John P.; Map Projections - A Working Manual; USGS Professional Paper 1395
;;;
;;; symbol mapping
;;; lambda -> longitude
;;; phi    -> latitude
;;; R      -> radius of the sphere (either actual or corresponding to the scale of the map)
;;; k0     -> relative scale factor along a parallel of latitude

;; TODO maybe add simplifications for long-0 or lat-0 = 0 or 90 degrees

(defn stereographic-projection
  "Calculates the stereographic projection of the coordinates of long and lat
   for a map centered on the coordinates long-0 and lat-0."
  ([R k-0 [long-0 lat-0] [long lat]]
   (stereographic-projection R k-0 long-0 lat-0 long lat))
  ([R k-0 long-0 lat-0 long lat]
   (let [k (/ (* 2 k-0)
              (+ 1
                 (* (m/sin lat-0) (m/sin lat))
                 (* (m/cos lat-0) (m/cos lat) (m/cos (- long long-0)))))
         x (* R k (m/cos lat) (m/sin (- long long-0)))
         y (* R k (- (* (m/cos lat-0) (m/sin lat))
                     (* (m/sin lat-0) (m/cos lat) (m/cos (- long long-0)))))]
           ;h-stroke (+ (* (sin lat-1) (sin lat)) (* (cos lat-1) (cos lat) (cos (- long long-0)))) ; scale
           ;k-stroke 1.0 ; scale
       ;[x y h-stroke k-stroke]
     [x y])))

(defn reverse-stereographic-projection
  "Calculates the coordinates of x and y in a reversed stereographic projection
   for a map centered on the coordinates long-0 and lat-0."
  ([R k-0 [long-0 lat-0] [x y]]
   (reverse-stereographic-projection R k-0 long-0 lat-0 x y))
  ([R k-0 long-0 lat-0 x y]
   (let [rho (m/sqrt (+ (* x x) (* y y)))
         c (* 2 (m/atan2 rho (* 2 R k-0)))
         ; c (* 2 (atan (/ rho (* 2 R k-0))))
         lat (if (= rho 0.0)
               lat-0
               (m/asin (+ (* (m/cos c) (m/sin lat-0))
                          (/ (* y (m/sin c) (m/cos lat-0))
                             rho))))
         long (cond
                (= rho 0.0) long-0
                (= lat-0 (/ m/PI 2)) (+ long-0 (m/atan2 x (* -1 y)))
                (= lat-0 (/ m/PI -2)) (+ long-0 (m/atan2 x y))
                :default (+ long-0 (m/atan (* x (m/sin (/ c
                                                          (- (* rho (m/cos lat-0) (m/cos c))
                                                             (* y (m/sin lat-0) (m/sin c)))))))))]
     [long lat])))

(defn stereographic-projector
  "Returns a function for stereographic projections."
  ([R]
   (partial stereographic-projection R))
  ([R k-0]
   (partial stereographic-projection R k-0))
  ([R k-0 [long-0 lat-0]]
   (partial stereographic-projection R k-0 [long-0 lat-0]))
  ([R k-0 long-0 lat-0]
   (partial stereographic-projection R k-0 long-0 lat-0)))

(defn reverse-stereographic-projector
  "Returns a function for reverse stereographic projections."
  ([R]
   (partial reverse-stereographic-projection R))
  ([R k-0]
   (partial reverse-stereographic-projection R k-0))
  ([R k-0 [long-0 lat-0]]
   (partial reverse-stereographic-projection R k-0 [long-0 lat-0]))
  ([R k-0 long-0 lat-0]
   (partial reverse-stereographic-projection R k-0 long-0 lat-0)))

(defn orthographic-projection
  "Calculates the orthographic projection of the coordinates of the coordinates of long and lat
   for a map centered on the coordinates long-0 and lat-0."
  ([R [long-0 lat-0] [long lat]]
   (orthographic-projection R long-0 lat-0 long lat))
  ([R long-0 lat-0 long lat]
   (let [x (* R (m/cos lat) (m/sin (- long long-0)))
         y (* R (- (* (m/cos lat-0) (m/sin lat))
                   (* (m/sin lat-0) (m/cos lat) (m/cos (- long long-0)))))]
         ;h-stroke (+ (* (sin lat-1) (sin lat)) (* (cos lat-1) (cos lat) (cos (- long long-0))))
         ;k-stroke 1.0

     ;[x y h-stroke k-stroke]
     [x y])))

(defn reverse-orthographic-projection
  "Calculates the coordinates of x and y in a reversed orthographic projection
   for a map centered on the coordinates long-0 and lat-0."
  ([R [long-0 lat-0] [x y]]
   (reverse-orthographic-projection R long-0 lat-0 x y))
  ([R long-0 lat-0 x y]
   (let [rho (m/sqrt (+ (* x x) (* y y)))
         c (m/asin (/ rho R))
         lat (if (= rho 0.0)
               lat-0
               (m/asin (+ (* (m/cos c) (m/sin lat-0))
                          (/ (* y (m/sin c) (m/cos lat-0))
                             rho))))
         long (cond
                (= rho 0.0) long-0
                (= lat-0 m/HALF-PI) (+ long-0 (m/atan2 x (* -1 y)))
                (= lat-0 (/ m/PI -2)) (+ long-0 (m/atan2 x y))
                :default (+ long-0 (m/atan (* x (m/sin (/ c
                                                          (- (* rho (m/cos lat-0) (m/cos c))
                                                             (* y (m/sin lat-0) (m/sin c)))))))))]
     [long lat])))

(defn orthographic-projector
  "Returns a function for orthographic projections."
  ([R]
   (partial orthographic-projection R))
  ([R [long-0 lat-0]]
   (partial orthographic-projection R [long-0 lat-0]))
  ([R long-0 lat-0]
   (partial orthographic-projection R long-0 lat-0)))

(defn reverse-orthographic-projector
  "Returns a function for reverse orthographic projections."
  ([R]
   (partial reverse-orthographic-projection R))
  ([R [long-0 lat-0]]
   (partial reverse-orthographic-projection R [long-0 lat-0]))
  ([R long-0 lat-0]
   (partial reverse-orthographic-projection R long-0 lat-0)))


;TODO implement other projections
(defn mercator-projection
  "Calculates the mercator projection of the coordinates."
  []
  (let []))


(defn reverse-mercator-projection
  "Calculates the coordinates in a reversed mercator projection."
  []
  (let []))

;;;
;;; Protocols and types for coordinates
;;;

(defprotocol ICoordinate
  "Protocol for coordinate systems."
  (equatorial [this] [this jd location] "Returns the equtorial coordinates (RA/Dec).")
  (horizontal [this] [this jd location] "Returns the horizontal coordinates (Alt/Az).") 
  (ecliptical [this] [this jd location] "Returns the ecliptical coordinates (lat/long).")
  ; (galactical [obj] [obj time] "Returns the galactical coordinates.")
  )

(defrecord EquatorialCoordinate [ra dec]
  ICoordinate
  (equatorial [this] [ra dec])
  (equatorial [this jd location])   ; TODO implement
  (horizontal [this])               ; TODO implement
  (horizontal [this jd location])   ; TODO implement
  (ecliptical [this])               ; TODO implement
  (ecliptical [this jd location]))  ; TODO implement

(defrecord HorizontalCoordinate [alt az]
  ICoordinate
  (equatorial [this])               ; TODO implement
  (equatorial [this jd location])   ; TODO implement
  (horizontal [this] [alt az])
  (horizontal [this jd location])   ; TODO implement
  (ecliptical [this])               ; TODO implement
  (ecliptical [this jd location]))  ; TODO implement

(defrecord EclipticalCoordinate [lat long]
  ICoordinate
  (equatorial [this])               ; TODO implement
  (equatorial [this jd location])   ; TODO implement
  (horizontal [this])               ; TODO implement
  (horizontal [this jd location])   ; TODO implement
  (ecliptical [this] [lat long])
  (ecliptical [this jd location]))  ; TODO implement
