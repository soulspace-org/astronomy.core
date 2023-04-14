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

(ns org.soulspace.astronomy.precession
  (:require [org.soulspace.math.core :as m]))

;;;
;;; Functions for calculating the precession
;;;
;;; References:
;;; Jean Meeus; Astronomical Algorithms, 2. Ed.; Willmann-Bell
;;;

(defn calc-m
  "Calculates m in seconds for the given time 'T'
   in julian centuries from J2000.0."
  [T]
  (+ 3.07496 (* 0.00186 T)))

(defn calc-n
  "Calculates n in seconds for the given time 'T'
   in julian centuries from J2000.0"
  [T]
  (- 1.33621 (* 0.00057 T)))

(defn annual-precession-low-accuracy
  "Calculates the annual precession with low accuracy for the given time 'T'
  in julian centuries from J2000.0.
  
  This formula may be used, if no great accuracy is required, the epochs
  are not to widely separated and if the position is not too close to
  one of the celestial poles."
  [T [ra dec]]
  (let [m (calc-m T)
        n (calc-n T)]
   [(+ m (* n (m/sin ra) (m/tan dec)))
    (* 15 n (m/cos ra))]))


(defn- calc-zeta
  "Calculates for the given time 'T'
   in julian centuries from J2000.0"
  [T t]
  (+ (* (+ 2306.2181 (* 1.39656 T) (* -0.000139 (m/sqr T))) t)
     (* (+ 0.30188 (* -0.000344 T)) (m/sqr t))
     (* 0.017998 (m/cube t))))

(defn- calc-z
  ""
  [T t]
  (+ (* (+ 2306.2181 (* 1.39656 T) (* -0.000139 (m/sqr T))) t)
     (* (+ 1.09468 (* 0.000066 T)) (m/sqr t))
     (* 0.018203 (m/cube t))))

(defn- calc-theta
  ""
  [T t]
  (- (* (+ 2004.3109 (* 0.85330 T) (* -0.000217 (m/sqr T))) t)
     (* (+ 0.42665 (* 0.000217 T)) (m/sqr t))
     (* 0.041833 (m/cube t))))

(defn precession
  "Calculates the precession for the given time 'T'
   in julian centuries from J2000.0 with high accuracy."
  ([t [ra dec]]
   (let [zeta  (+ (* 2306.2181 t) (* 0.30188 (m/sqr t)) (* 0.017998 (m/cube t)))
         z     (+ (* 2306.2181 t) (* 1.09468 (m/sqr t)) (* 0.018203 (m/cube t)))
         theta (- (* 2004.3109 t) (* 0.42665) (m/sqr t) (* 0.041833 (m/cube t)))]))
  ([T t [ra dec]]
   (let [zeta  (calc-zeta T t)
         z     (calc-z T t)
         theta (calc-theta T t)]))
  ([T t zeta z theta [ra dec]]
   (let [A (* (m/cos dec) (m/sin (+ ra zeta)))
         B (- (* (m/cos theta) (m/cos dec) (m/cos (+ ra zeta))) (* (m/sin theta) (m/sin dec)))
         C (+ (* (m/sin theta) (m/cos dec) (m/cos (+ ra zeta))) (* (m/cos theta) (m/sin dec)))
         a (+ (m/atan2 A B) z)
         d (if (< 1.4 (abs dec))
                 ; near the pole
                 (m/acos (m/sqrt (+ (m/sqr A) (m/sqr B))))
                 ; else
                 (m/asin C))]
     [a d])))
