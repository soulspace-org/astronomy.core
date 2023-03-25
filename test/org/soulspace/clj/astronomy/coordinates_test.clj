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

(ns org.soulspace.clj.astronomy.coordinates-test
  (:require [clojure.test :refer :all]
            [org.soulspace.math.core :as m]
            [org.soulspace.clj.astronomy.coordinates.coordinates :refer :all]
            [org.soulspace.clj.astronomy.angle :as a]
            [org.soulspace.clj.astronomy.test-utils :as utils]))

; Arcturus
(def ra1 (m/deg-to-rad (a/ha-to-deg (a/hms-to-ha "14h15m39.7s"))))
(def dec1 (m/deg-to-rad (a/dms-to-deg "19°10'57\"")))
;(def ra1 (deg-to-rad 213.9154))
;(def dec1 (deg-to-rad 19.1825))

; Spica
(def ra2 (m/deg-to-rad (a/ha-to-deg (a/hms-to-ha "13h25m11.6s"))))
(def dec2 (m/deg-to-rad (a/dms-to-deg "-11°09'41\"")))
;(def ra2 (deg-to-rad 201.2983))
;(def dec2 (deg-to-rad -11.1614))


(deftest angular-distance-test
  (is (= 0.0 (angular-distance [0 0] [0 0])))
  (is (= 0.0 (angular-distance [1 1] [1 1])))
  (is (= 0.0 (angular-distance [4 4] [4 4])))
  (is (= 1.0 (angular-distance [1 1] [1 2])))
  (is (= 1.0 (angular-distance [1 2] [1 1])))
  (is (= 1.0 (angular-distance [1 2] [1 1])))
  (is (= (m/deg-to-rad 180)
         (angular-distance [(m/deg-to-rad 90) (m/deg-to-rad 0)]
                           [(m/deg-to-rad -90) (m/deg-to-rad 0)])))
  (is (= (m/deg-to-rad 0)
         (angular-distance [(m/deg-to-rad -180) (m/deg-to-rad 0)]
                           [(m/deg-to-rad 180) (m/deg-to-rad 0)])))
  (is (= (m/deg-to-rad 180)
         (angular-distance [(m/deg-to-rad -180) (m/deg-to-rad 0)]
                           [(m/deg-to-rad 360) (m/deg-to-rad 0)])))
  (is (= (m/deg-to-rad 0)
         (angular-distance [(m/deg-to-rad -180) (m/deg-to-rad 0)]
                           [(m/deg-to-rad 540) (m/deg-to-rad 0)])))
  (is (= (m/deg-to-rad 180)
         (angular-distance [(m/deg-to-rad -180) (m/deg-to-rad 90)]
                           [(m/deg-to-rad 180) (m/deg-to-rad -90)])))
  (is (= (m/deg-to-rad 180)
         (angular-distance [(m/deg-to-rad 0) (m/deg-to-rad 90)]
                           [(m/deg-to-rad 0) (m/deg-to-rad -90)])))
  (is (utils/within-error-margin (m/deg-to-rad 20)
                         (angular-distance [(m/deg-to-rad 10) (m/deg-to-rad 0)]
                                           [(m/deg-to-rad -10) (m/deg-to-rad 0)])))
  (is (utils/within-error-margin 32.7930
                         (m/rad-to-deg (angular-distance [ra1 dec1]
                                                         [ra2 dec2]))))) ; Arcturus <-> Spica

(deftest altitude-by-zenit-distance-tests
  (is (= (m/deg-to-rad 90) (altitude-by-zenit-distance (m/deg-to-rad 0))))
  (is (= (m/deg-to-rad 0)  (altitude-by-zenit-distance (m/deg-to-rad 90))))
  (is (= (m/deg-to-rad 80) (altitude-by-zenit-distance (m/deg-to-rad 10))))
  (is (= (m/deg-to-rad 45) (altitude-by-zenit-distance (m/deg-to-rad 45)))))

(deftest zenit-distance-by-altitude-test
  (is (= (m/deg-to-rad 90) (zenit-distance-by-altitude (m/deg-to-rad 0))))
  (is (= (m/deg-to-rad 0)  (zenit-distance-by-altitude (m/deg-to-rad 90))))
  (is (= (m/deg-to-rad 80) (zenit-distance-by-altitude (m/deg-to-rad 10))))
  (is (= (m/deg-to-rad 45) (zenit-distance-by-altitude (m/deg-to-rad 45)))))

;(run-tests)
