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

(ns org.soulspace.astronomy.coordinates-test
  (:require [clojure.test :refer :all]
            [org.soulspace.math.core :as m]
            [org.soulspace.astronomy.coordinates :refer :all]
            [org.soulspace.astronomy.angle :as a]
            [org.soulspace.astronomy.time :as t]
            [org.soulspace.astronomy.test-utils :as utils]))

; Arcturus
(def Arcturus [(a/hms-to-rad "14h15m39.7s")
               (a/dms-to-rad "19°10'57\"")])
; Spica
(def Spica [(a/hms-to-rad "13h25m11.6s")
            (a/dms-to-rad "-11°09'41\"")])

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
 ; Arcturus <-> Spica
  (is (utils/within-error-margin 32.7930
                                 (m/rad-to-deg
                                  (angular-distance Arcturus Spica)))))

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

(def theta-Persei [(a/hms-to-rad "2h44m11.968s")
                   (a/dms-to-rad "49°13'42.48\"")])

(def theta-Persei-PM [(a/hms-to-rad "0h0m0.03425s")
                      (a/dms-to-rad "-0°0'0.0895\"")])

(deftest proper-motion-test
  (testing "Proper motion of theta Persei"
    (let [[ra dec] (proper-motion 28.86705 theta-Persei theta-Persei-PM)]
      (is (utils/within-error-margin 41.054063 (m/rad-to-deg ra)))
      (is (utils/within-error-margin 49.227750 (m/rad-to-deg dec))))))

(comment
  (* 28.86705 0.03425)
  (* 28.86705 -0.0895)
  (run-tests)
  )
