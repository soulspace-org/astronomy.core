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

(ns org.soulspace.astronomy.topology
  (:require [org.soulspace.math.core :as mc]))
#?(:clj (require '[clojure.math :as m])
   :cljs (require '[cljs.math :as m]))

;;;
;;; Functions for topological calculations.
;;;
;;; The equatorial and polar radius are expected as meters.
;;; 
;;; References:
;;; Jean Meeus; Astronomical Algorithms, 2. Ed.; Willmann-Bell
;;;

(def location-types #{:topographic :topocentric})

(defn polar-radius
  "Calculates the polar-radius from the equatorial-radius and the flattening."
  ^double [^double equatorial-radius ^double flattening]
  (* equatorial-radius (- 1 flattening)))

(defn eccentricity
  "Calculates the eccentricity of the meridian from the given flattening."
  ^double [^double flattening]
  (m/sqrt (- (* 2 flattening) (mc/sqr flattening))))

(defn topocentric-latitude
  "Calculates the topocentric latitude for the given topographic latitude."
  ^double [^double topographic-latitude ^double equatorial-radius ^double polar-radius]
  (m/atan (* (/ (mc/sqr equatorial-radius ) (mc/sqr polar-radius))
           (m/tan topographic-latitude))))

(defn topocentric-parameters-by-height
  [^double topographic-latitude ^double height ^double equatorial-radius ^double polar-radius]
  (let [u (m/atan (* (/ polar-radius equatorial-radius) (m/tan topographic-latitude)))
        rho-sin-gc-lat (+ (* (/ polar-radius equatorial-radius) (m/sin u)) (* (/ height equatorial-radius) (m/sin topographic-latitude)))
        rho-cos-gc-lat (+ (m/cos u)(* (/ height equatorial-radius) (m/cos topographic-latitude)))
        rho (if (> (abs topographic-latitude) (/ m/PI 4))
              (/ rho-sin-gc-lat (m/sin (topocentric-latitude topographic-latitude equatorial-radius polar-radius)))
              (/ rho-cos-gc-lat (m/cos (topocentric-latitude topographic-latitude equatorial-radius polar-radius))))]
    {:u u :rho rho :rho-sin-topocentric-lat rho-sin-gc-lat :rho-cos-topocentric-lat rho-cos-gc-lat}))

(defn topocentric-distance
  "Calculates the distance of the center of the body in equatorial radiuses."
  ^double [^double topographic-latitude ^double height ^double equatorial-radius ^double polar-radius]
  (:rho (topocentric-parameters-by-height topographic-latitude height equatorial-radius polar-radius)))

(defn parallel-radius
  "Calculates the radius of the parallel circle at the given topographic latitude."
  ^double [^double topographic-latitude ^double equatorial-radius ^double eccentricity]
  (/ (* equatorial-radius (m/cos topographic-latitude))
     (m/sqrt (- 1 (* (mc/sqr eccentricity) (mc/sqr (m/sin topographic-latitude)))))))

(defn longitude-distance-per-degree
  "Calculates the distance per degree of longitude for the given topographic latitude."
  ^double [^double topographic-latitude ^double equatorial-radius ^double eccentricity]
  (* (/ m/PI 180) (parallel-radius topographic-latitude equatorial-radius eccentricity)))

(defn curvature-radius
  "Calculates the curvature radius for the given topographic latitude."
  [^double topographic-latitude ^double equatorial-radius ^double eccentricity]
  (/ (* equatorial-radius (- 1 (mc/sqr eccentricity)))
     (m/pow (- 1 (* (mc/sqr eccentricity) (mc/sqr (m/sin topographic-latitude)))) 3/2)))

(defn latitude-distance-per-degree
  "Calculates the distance per degree of latitude for the given topographic latitude."
  ^double [^double topographic-latitude ^double equatorial-radius ^double eccentricity]
  (* (/ m/PI 180) (curvature-radius topographic-latitude equatorial-radius eccentricity)))

(defn linear-velocity
  "Calculates the linear velocity with respect to the stars at the given latitude in meters per second.
  Omega is the rotational angular velocity with respect to the stars at the epoch."
  ^double [^double topographic-latitude ^double equatorial-radius ^double eccentricity ^double omega]
  (* omega (parallel-radius topographic-latitude equatorial-radius eccentricity)))

(defn topodesic-distance
  "Calculates the topodesic distance (great circle distance) between 2 positions on the body."
  (^double [[long1 lat1] [long2 lat2] ^double equatorial-radius ^double flattening]
   (topodesic-distance long1 lat1 long2 lat2 equatorial-radius flattening))
  ([long1 lat1 long2 lat2 equatorial-radius flattening]
   ; (^double [long1 lat1 long2 lat2 ^double equatorial-radius ^double flattening]
   ; fns taking primitives support only 4 or fewer args
   (let [F (/ (+ lat1 lat2) 2)
         G (/ (- lat1 lat2) 2)
         L (/ (+ long1 long2) 2.0)
         S (+ (* (mc/sqr (m/sin G)) (mc/sqr (m/cos L))) (* (mc/sqr (m/cos F)) (mc/sqr (m/sin L))))
         C (+ (* (mc/sqr (m/cos G)) (mc/sqr (m/cos L))) (* (mc/sqr (m/sin F)) (mc/sqr (m/sin L))))
         w (m/atan (m/sqrt (/ S C)))
         R (/ (m/sqrt (* S C)) w)
         D (* 2 w equatorial-radius)
         H1 (/ (- (* 3 R) 1) (* 2 C))
         H2 (/ (+ (* 3 R) 1) (* 2 S))
         s (* D (+ 1 (* flattening H1 (mc/sqr (m/sin F)) (mc/sqr (m/cos G))) (* -1 flattening H2 (mc/sqr (m/cos F)) (mc/sqr (m/sin G)))))]
     s)))

(defprotocol ITopologicalCoordinate
  )

(defprotocol ITopologicalBody
  )
