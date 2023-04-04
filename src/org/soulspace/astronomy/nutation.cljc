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

(ns org.soulspace.astronomy.nutation
  (:require [org.soulspace.math.core :as m]
            [org.soulspace.astronomy.time :as time]
            [org.soulspace.astronomy.angle :as angle]))

;;;;
;;;; Functions for the calculation nutation and the obliquity of the ecliptic.
;;;;
;;;; References:
;;;; Jean Meeus; Astronomical Algorithms, 2. Ed.; Willmann-Bell
;;;;

(defn mean-elongation-moon-from-sun
  "Calculates the mean elongation of the moon from the sun in degrees
  at the given instant 't' in julian centuries."
  [t]
  (+ 297.85036M (* 445267.111480M t) (* -1 0.0019142 t t) (/ (* t t t)
                                                             189474)))

(defn mean-longitude-sun
  "Calculates the mean longitude of the sun in degrees at the given
  instant 't' in julian centuries."
  [t]
  (+ 280.4665M (* 36000.7698M t)))

(defn mean-longitude-moon
  "Calculates the mean longitude of the moon in degrees at the given
  't' instant in julian centuries."
  [t]
  (+ 218.3165 (* 481267.8813 t)))

(defn mean-anomaly-sun
  "Calculates the mean anomaly of the sun in degrees at the given
  instant 't' in julian centuries."
  [t]
  (+ 357.52772M (* 35999.050340M t) (* -1 0.0001603M t t) (/ (* -1 t t t)
                                                             300000.0M)))

(defn mean-anomaly-moon
  "Calculates the mean anomaly of the moon in degrees at the given
  instant 't' in julian centuries."
  [t]
  (+ 134.96298M (* 477198.867398M t) (* 0.0086972M t t) (/ (* t t t)
                                                           56250M)))

(defn argument-of-latitude-moon
  "Calculates the argument of latitude for the moon in degrees at the
  given instant 't' in julian centuries."
  [t]
  (+ 93.27191M (* 483202.017538M t) (* -1 0.0036825M t t) (/ (* t t t)
                                                             327270M)))

(defn longitude-ascending-node-moon
  "Calculates the longitude of ascending node of the moons mean orbit on the
  ecliptic in degrees at the given instant 't' in julian centuries, measured
  from the mean equinox of the date."
  [t]
  (+ 125.04452M (* -1934.136261M t) (* 0.0020708M t) (/ (* t t t)
                                                        450000M)))

(defn nutation-in-longitude
  "Calculates the nutation in longitude (delta psi) in arc seconds for the
  julian ephemerides day with an accuracy of 0.5 arc seconds. "
  [jde]
  (let [t (time/julian-centuries jde)
        omega (longitude-ascending-node-moon t)
        l-sun (mean-longitude-sun t)
        l-moon (mean-longitude-moon t)]
    ; TODO test
    (+ (* -17.20 (m/sin omega)) (* -1.32 (m/sin (* 2 l-sun)))
       (* -0.23 (m/sin (* 2 l-moon))) (* 0.21 (m/sin (* 2 omega))))))

(defn nutation-in-obliquity
  "Calculates the nutation in obliquity (delta epsilon) in arc seconds for the
  julian ephemerides day with an accuracy of 0.1 arc seconds."
  [jde]
  (let [t (time/julian-centuries jde)
        omega (longitude-ascending-node-moon t)
        l-sun (mean-longitude-sun t)
        l-moon (mean-longitude-moon t)]
    ; TODO test
    (+ (* 9.20 (m/cos omega)) (* 0.57 (m/cos (* 2 l-sun)))
       (* 0.10 (m/cos (* 2 l-moon))) (* 0.09 (m/cos (* 2 omega))))))

(def ^:const obliquity_B1950 23.4457889M)
(def ^:const obliquity_J2000 23.4392911M)

(defn mean-obliquity
  "Calculates the mean obliquity of the ecliptic (the inclination of the
  earth's axis) for the julian ephemerides day. The error is about
  1 arc second over a period of 2000 years and 10 arc seconds over a
  period of 4000 years."
  [jde]
  (let [t (time/julian-centuries jde)]
        ; TODO convert to deg and use the deg values here
       (+ obliquity_J2000
          (* -1 (angle/dms-to-deg "0°0'46.8150\"") t)
          (* -1 (angle/dms-to-deg "0°0'0.00059\"") t t)
          (* (angle/dms-to-deg "0°0'0.001813\"") t t t))))

(defn mean-obliquity-high-accuracy
  "Calculates the mean obliquity of the ecliptic (the inclination of the
  earth's axis) for the julian ephemerides day with high accuracy.
  The error is about 0.01 arc second over a period of 2000 years and
  a few arc seconds over a period of 20000 years."
  [jde]
  (let [u (/ (time/julian-centuries jde)
             100)]
    (+ obliquity_J2000
       (* -1 (angle/dms-to-deg "0°0'4680.93\"") u)
       (* -1 (angle/dms-to-deg "0°0'1.55\"")    u u)
       (*    (angle/dms-to-deg "0°0'1999.25\"") u u u)
       (* -1 (angle/dms-to-deg "0°0'51.38\"")   u u u u)
       (* -1 (angle/dms-to-deg "0°0'249.67\"")  u u u u u)
       (* -1 (angle/dms-to-deg "0°0'39.05\"")   u u u u u u)
       (*    (angle/dms-to-deg "0°0'7.12\"")    u u u u u u u)
       (*    (angle/dms-to-deg "0°0'27.87\"")   u u u u u u u u)
       (*    (angle/dms-to-deg "0°0'5.79\"")    u u u u u u u u u)
       (*    (angle/dms-to-deg "0°0'2.45\"")    u u u u u u u u u u))))

(defn true-obliquity
  "Calculates the true obliquity of the ecliptic (the inclination of the
  earth's axis) for the julian ephemerides day. The error is about
  1 arc second over a period of 2000 years and 10 arc seconds over a
  period of 4000 years."
  [jde]
  (+ (mean-obliquity jde) (nutation-in-obliquity jde)))

(comment
(defn true-obliquity-high-accuracy
  "Calculates the mean obliquity of the ecliptic (the inclination of the
  earth's axis) for the julian ephemerides day with high accuracy.
  The error is about 0.01 arc second over a period of 2000 years and
  a few arc seconds over a period of 20000 years."
  [jde]
  ; TODO implement high accuracy variant of nutation functions
  (+ (mean-obliquity-high-accuracy jde) (nutation-in-obliquity jde)))
)

(comment
  (mean-longitude-sun (time/julian-centuries 2446895.5))
  (mean-longitude-moon (time/julian-centuries 2446895.5))
  (mean-anomaly-sun (time/julian-centuries 2446895.5))
  (mean-anomaly-moon (time/julian-centuries 2446895.5))
  (argument-of-latitude-moon (time/julian-centuries 2446895.5))
  (longitude-ascending-node-moon (time/julian-centuries 2446895.5))
  (nutation-in-longitude 2446895.5)
  (nutation-in-obliquity 2446895.5)
  (angle/deg-to-dms (mean-obliquity 2446895.5)) 
  )