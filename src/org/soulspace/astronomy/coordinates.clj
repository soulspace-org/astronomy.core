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
            [org.soulspace.astronomy.time :as time]
            [org.soulspace.astronomy.angle :as a]))

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

;; location in orbital plane 
(defn location-orbital-plane-spherical
  "Returns the location of a body in spherical coordinates
  of the plane of it's orbit given the cartesian coordinates 'x' and 'y'."
  [x y]
  ; TODO implement
  ;((m/acos (x))) (())
  )

(defn location-orbital-plane-cartesian
  "Returns the location of a body in cartesian coordinates
   of the plane of it's orbit given 'r' as the distance from the sun
   and 'u' as the angle from the ascending node."
  [r u]
  [(* r (m/cos u))(* r (m/sin u))])

;;;
;;; Implementations of spherical projections, no ellipsoid projections implemented yet
;;;
;;; References:
;;; Snyder, John P.; Map Projections - A Working Manual; USGS Professional Paper 1395
;;;
;;; symbol mapping
;;; lambda -> longitude
;;; phi    -> latitude
;;; R      -> radius of the sphere (either actual or corresponding to the scale of the map)
;;; k0     -> relative scale factor along a parallel of latitude

;; TODO add simplifications for long-0 or lat-0 = 0 or 90 degrees

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
  "Returns a partial function for performing a stereographic projection of the
   coordinates of long and lat for a map centered on the coordinates long-0 and lat-0."
  ([R]
   (partial stereographic-projection R))
  ([R k-0]
   (partial stereographic-projection R k-0))
  ([R k-0 [long-0 lat-0]]
   (partial stereographic-projection R k-0 [long-0 lat-0]))
  ([R k-0 long-0 lat-0]
   (partial stereographic-projection R k-0 long-0 lat-0)))

(defn reverse-stereographic-projector
  "Returns a partial function for reverse stereographic projection of x and y
   in a reversed stereographic projection for a map centered on the coordinates
   long-0 and lat-0."
  ([R]
   (partial reverse-stereographic-projection R))
  ([R k-0]
   (partial reverse-stereographic-projection R k-0))
  ([R k-0 [long-0 lat-0]]
   (partial reverse-stereographic-projection R k-0 [long-0 lat-0]))
  ([R k-0 long-0 lat-0]
   (partial reverse-stereographic-projection R k-0 long-0 lat-0)))

(defn orthographic-projection
  "Calculates the orthographic projection of the coordinates of the coordinates
   of long and lat for a map centered on the coordinates long-0 and lat-0."
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
  "Returns a partial function for orthographic projection of the coordinates
   of long and lat for a map centered on the coordinates long-0 and lat-0."
  ([R]
   (partial orthographic-projection R))
  ([R [long-0 lat-0]]
   (partial orthographic-projection R [long-0 lat-0]))
  ([R long-0 lat-0]
   (partial orthographic-projection R long-0 lat-0)))

(defn reverse-orthographic-projector
  "Returns a partial function for reverse orthographic projection of x and y
   in a reversed stereographic projection for a map centered on the coordinates
   long-0 and lat-0."
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
;;; Apparent place of a star
;;;
;;; apply proper motion
;;; apply precession
;;; apply annual abberation
;;; (apply annual parallax)
;;; (apply gravitational deflection of light)

(defn proper-motion
  "Returns the new ccordinates of a star with proper motion applied.

   'T'  - the epoch for which the coordinates are given,
   't'  - the epoch for which the coordinates should be calculated,
   'dt' - the delta t in years for which the coordinates should be calculated,
   'c'  - vector of coordinates at epoch 'T' in the form ['ra' 'dec'],
   'pm' - vector of proper motion per year in the form ['pm-ra' 'pm-dec']"
  ([dt [ra dec] [pm-ra pm-dec]]
   [(+ ra (* dt pm-ra)) (+ dec (* dt pm-dec))])
  ([T t c pm]
   (proper-motion (- t T) c pm)))

(defn eccentricity-earth-orbit
  "Calculates the eccentricity of the orbit of the earth
   for the given time 'T' in julian centuries from J2000.0."
  [T]
  (- 0.016708634 (* 0.000042037 T) (* 0.0000001267 (m/sqr T))))

(defn mean-longitude-sun
  "Calculates the mean longitude of the sun in degrees
   for the given time 'T' in julian centuries from J2000.0."
  [T]
  (+ 280.46646 (* 36000.76983 T) (* 0.0003032 (m/sqr T))))

(defn mean-anomaly-sun
  "Calculates the mean anomaly of the sun in degrees 
   for the given time 'T' in julian centuries from J2000.0."
  [T]
  (+ 357.52911 (* 35999.05029 T)  (* -0.0001537 (m/sqr T))))

(defn center-of-sun
  "Calculates the equation of the center of the sun in degrees 
   for the given mean anomaly of the sun 'M' and
   the given time 'T' in julian centuries from J2000.0."
  [M T]
  (let [m (m/deg-to-rad M)]
    (+ (* (- 1.914602 (* 0.004817 T) (* 0.000014 (m/sqr T))) (m/sin m))
       (* (- 0.019993 (* 0.000101 T)) (m/sin (* 2 m)))
       (* 0.000289 (m/sin (* 3 m))))))

(defn true-longitude-of-sun
  "Calculates the true (geometric) longitude of the sun in degrees 
   for the given time 'T' in julian centuries from J2000.0."
  [T]
  (let [L0 (mean-longitude-sun T)
        M  (mean-anomaly-sun T)
        C  (center-of-sun M T)]
    (+ L0 C)))

(defn true-anomaly-of-sun
  "Calculates the true anomaly of the sun in degrees 
   for the given time 'T' in julian centuries from J2000.0."
  [T]
  (let [M  (mean-anomaly-sun T)
        C  (center-of-sun M T)]
    (+ M C)))

(defn distance-sun-earth
  "Calculates the radius vector R, the distance between the centers of sun and earth
   in astronomical units for the given time 'T' in julian centuries from J2000.0
   and the eccentricity 'e' of the orbit of the earth and the true anomaly
   of the sun 'v'."
  ([T]
   (distance-sun-earth (eccentricity-earth-orbit T) (true-anomaly-of-sun T) T))
  ([e v T]
   (/ (* 1.000001018 (- 1 (m/sqr e)))
      (+ 1 (* e (m/cos v))))))

(defn longitude-earth-perihelion
  "Calculates the longitude of the perihelion of the orbit of the earth
  in degrees for the given time 'T' in julian centuries from J2000.0."
  [T]
  (+ 102.93735 (* 1.71946 T) (* 0.00046 (m/sqr T))))

(def ^:const kappa "Constant of abberation [°]]." (a/dms-to-rad "0°0'20.49552\""))

(defn annual-abberation-ecliptical
  "Calculates the annual abberation in rad for the given time 'T'
   in julian centuries from J2000.0. and the ecliptical coordinates
   given as a vector 'v' in the form of ['lat' 'long'] in rad."
  ([T [lat long]]
   (let [e (eccentricity-earth-orbit T)
         l-sun (m/deg-to-rad (true-longitude-of-sun T))
         pi (m/deg-to-rad (longitude-earth-perihelion T))
         d-long (/ (+ (* -1 kappa (m/cos (- l-sun long)))
                      (* e kappa (m/cos (- pi long))))
                   (m/cos lat))
         d-lat (* -1 kappa (m/sin lat)
                   (- (m/sin (- l-sun long))
                      (* e (m/sin (- pi long)))))]
     [d-lat d-long])))

(defn annual-abberation-equatorial
  "Calculates the annual abberation in rad for the given time 'T'
   in julian centuries from J2000.0 and the equatorial coordinates
   given as a vector 'v' in the form of ['ra' 'dec'] in rad."
  ([T [ra dec]]
   (let [e (eccentricity-earth-orbit T)
         l-sun (m/deg-to-rad (true-longitude-of-sun T))
         pi (m/deg-to-rad (longitude-earth-perihelion T))]
     ; TODO
     [])))

(defn apparent-star-position
  ""
  [T c]
  )

;;;
;;; Protocols and types for coordinates
;;;

(defprotocol ICelestialCoordinate
  "Protocol for coordinate systems."
  (equatorial [this] [this jd location] "Returns the equtorial coordinates (RA/Dec).")
  (horizontal [this] [this jd location] "Returns the horizontal coordinates (Alt/Az).") 
  (ecliptical [this] [this jd location] "Returns the ecliptical coordinates (lat/long).")
  ; (galactical [obj] [obj time] "Returns the galactical coordinates.")
  )

(comment ; TODO implement analog to Angle and Distance
  (defrecord CelestialCoordinate [value unit]
    ICelestialCoordinate)
 )
