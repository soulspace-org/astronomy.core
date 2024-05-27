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
            [clojure.math :as m]
            [org.soulspace.astronomy.coordinates :refer :all]
            [org.soulspace.astronomy.angle :as a]
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
  (is (= (m/to-radians 180)
         (angular-distance [(m/to-radians 90) (m/to-radians 0)]
                           [(m/to-radians -90) (m/to-radians 0)])))
  (is (= (m/to-radians 0)
         (angular-distance [(m/to-radians -180) (m/to-radians 0)]
                           [(m/to-radians 180) (m/to-radians 0)])))
  (is (= (m/to-radians 180)
         (angular-distance [(m/to-radians -180) (m/to-radians 0)]
                           [(m/to-radians 360) (m/to-radians 0)])))
  (is (= (m/to-radians 0)
         (angular-distance [(m/to-radians -180) (m/to-radians 0)]
                           [(m/to-radians 540) (m/to-radians 0)])))
  (is (= (m/to-radians 180)
         (angular-distance [(m/to-radians -180) (m/to-radians 90)]
                           [(m/to-radians 180) (m/to-radians -90)])))
  (is (= (m/to-radians 180)
         (angular-distance [(m/to-radians 0) (m/to-radians 90)]
                           [(m/to-radians 0) (m/to-radians -90)])))
  (is (utils/within-error-margin (m/to-radians 20)
                         (angular-distance [(m/to-radians 10) (m/to-radians 0)]
                                           [(m/to-radians -10) (m/to-radians 0)])))
 ; Arcturus <-> Spica
  (is (utils/within-error-margin 32.7930
                                 (m/to-degrees
                                  (angular-distance Arcturus Spica)))))

(deftest altitude-by-zenit-distance-tests
  (is (= (m/to-radians 90) (altitude-by-zenit-distance (m/to-radians 0))))
  (is (= (m/to-radians 0)  (altitude-by-zenit-distance (m/to-radians 90))))
  (is (= (m/to-radians 80) (altitude-by-zenit-distance (m/to-radians 10))))
  (is (= (m/to-radians 45) (altitude-by-zenit-distance (m/to-radians 45)))))

(deftest zenit-distance-by-altitude-test
  (is (= (m/to-radians 90) (zenit-distance-by-altitude (m/to-radians 0))))
  (is (= (m/to-radians 0)  (zenit-distance-by-altitude (m/to-radians 90))))
  (is (= (m/to-radians 80) (zenit-distance-by-altitude (m/to-radians 10))))
  (is (= (m/to-radians 45) (zenit-distance-by-altitude (m/to-radians 45)))))

(def Regulus [(a/hms-to-rad "10h08m22.3s")
              (a/dms-to-rad "11°58'02\"")])

(def Regulus-PM [(a/hms-to-rad "-0h0m0.0169s")
                 (a/dms-to-rad "0°0'0.006\"")])

(def theta-Persei [(a/hms-to-rad "2h44m11.968s")
                   (a/dms-to-rad "49°13'42.48\"")])

(def theta-Persei-PM [(a/hms-to-rad "0h0m0.03425s")
                      (a/dms-to-rad "-0°0'0.0895\"")])

; TODO check
(deftest proper-motion-test
  (let [reg (proper-motion -22 Regulus Regulus-PM)
        theta (proper-motion 28.86705 theta-Persei theta-Persei-PM)]
    (testing "Proper motion of Regulus"
      (is (utils/within-error-margin 152.092917 (m/to-degrees (first reg)) 0.00001))
      (is (utils/within-error-margin 11.967186 (m/to-degrees (second reg)) 0.00001)))
    (testing "Proper motion of theta Persei"
      (is (utils/within-error-margin 41.054063 (m/to-degrees (first theta))) 0.00001)
      (is (utils/within-error-margin 49.227750 (m/to-degrees (second theta)) 0.00001)))))

(comment
  (* -22 -0.0169)
  (* -22 0.006)
  (* 28.86705 0.03425)
  (* 28.86705 -0.0895)
  (run-tests)
  )
