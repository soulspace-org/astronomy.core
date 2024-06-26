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

(ns org.soulspace.astronomy.topology-test
  (:require [clojure.test :refer :all]
            [org.soulspace.astronomy.test-utils :as utils]
            [org.soulspace.astronomy.angle :as a]
            [org.soulspace.astronomy.topology :refer :all]))
#?(:clj (require '[clojure.math :as m])
   :cljs (require '[cljs.math :as m]))

(def flattening (/ 1 298.257)) ; flattening

(def earth
  (let [equatorial-radius 6378140 ; equatorial radius in meters
        flattening (/ 1 298.257) ; flattening
        polar-radius (polar-radius equatorial-radius flattening) ; polar radius in meters
        eccentricity (eccentricity flattening) ; eccentricity of the meridian
        omega 7.292114992e-5] ; rotational angular velocity with respect to the stars at epoch 1996.5 (but earth is slowing down)
    {:equatorial-radius equatorial-radius
     :flattening flattening
     :polar-radius polar-radius
     :eccentricity eccentricity
     :omega omega}))
  
(deftest polar-radius-test
  (is (utils/within-error-margin 6356755.288
                         (polar-radius (:equatorial-radius earth)
                                       (:flattening earth))
                         0.001)))

(deftest eccentricity-test
  (is (utils/within-error-margin 0.08181922 (eccentricity flattening))))

; Location and height of the Palomar Observatory
(deftest topocentric-parameters-by-height-test
  (let [lat (m/to-radians (a/dms-to-deg "33° 21' 22\""))
        equatorial-radius (:equatorial-radius earth)
        polar-radius (:polar-radius earth)
        p (topocentric-parameters-by-height lat 1706.0 equatorial-radius polar-radius)]
    (is (utils/within-error-margin 33.267796 (m/to-degrees (:u p))))
    (is (utils/within-error-margin  0.546861 (:rho-sin-topocentric-lat p)))
    (is (utils/within-error-margin  0.836339 (:rho-cos-topocentric-lat p) ))))
