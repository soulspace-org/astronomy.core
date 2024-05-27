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

(ns org.soulspace.astronomy.angle-test
  (:require [clojure.test :refer :all]
            [clojure.math :as m]
            [org.soulspace.math.core :as mc]
            [org.soulspace.astronomy.test-utils :as utils]
            [org.soulspace.astronomy.angle :as angle]))

(deftest dms-to-deg-test
  (is (= 180.0     (angle/dms-to-deg "+180°")))
  (is (=   1.0     (angle/dms-to-deg "1°")))
  (is (=   0.5     (angle/dms-to-deg "0° 30'")))
  (is (=   0.0125  (angle/dms-to-deg "0° 0' 45\"")))
  (is (=  -0.0125  (angle/dms-to-deg "-0° 0' 45.0\"")))
  (is (utils/within-error-margin  19.1825 (angle/dms-to-deg "19° 10' 57\"")))
  (is (utils/within-error-margin -11.1614 (angle/dms-to-deg "-11° 09' 41\""))))

(deftest deg-to-dms-test
  (is (= {:sign 1 :deg 0 :min 0 :sec 45.0}  (angle/deg-to-dms 0.0125)))
  (is (= {:sign 1 :deg 0 :min 30 :sec 0.0}  (angle/deg-to-dms 0.5)))
  (is (= {:sign 1 :deg 1 :min 0 :sec 0.0}   (angle/deg-to-dms 1.0)))
  (is (= {:sign 1 :deg 18 :min 30 :sec 0.0} (angle/deg-to-dms 18.5))))

(deftest hms-to-ha-test
  (is (== 1.0 (angle/hms-to-ha "1h 0m 0s")))
  (is (== 2.5 (angle/hms-to-ha "2h 30m 0.0s")))
  (is (utils/within-error-margin 213.9154 (angle/ha-to-deg (angle/hms-to-ha "14h15m39.7s"))))
  (is (utils/within-error-margin 201.2983 (angle/ha-to-deg (angle/hms-to-ha "13h25m11.6s")))))

(deftest ha-to-hms-tests
  (is (= {:h 1 :min 0 :sec 0.0} (angle/ha-to-hms 1.0)))
  (is (= {:h 2 :min 30 :sec 0.0} (angle/ha-to-hms 2.5))))

(deftest ha-to-deg-test
  (is (= 180.0  (angle/ha-to-deg 12.0)))
  (is (= -180.0 (angle/ha-to-deg -12.0)))
  (is (= 7.5    (angle/ha-to-deg 0.5)))
  (is (= -7.5   (angle/ha-to-deg -0.5)))
  (is (= 375.0  (angle/ha-to-deg 25.0))))

(deftest deg-to-ha-test
  (is (= 12.0 (angle/deg-to-ha 180.0)))
  (is (= 12.0 (angle/deg-to-ha -180.0)))
  (is (= 0.5  (angle/deg-to-ha 7.5)))
  (is (= 23.5 (angle/deg-to-ha -7.5)))
  (is (= 1.0  (angle/deg-to-ha 375.0))))

(def deg-180 (angle/->Angle 180.0 :org.soulspace.astronomy.angle/deg))
(def deg-540 (angle/->Angle 540.0 :org.soulspace.astronomy.angle/deg))

(deftest degree-angle-tests
  (is (= m/PI  (angle/as-value deg-180 :org.soulspace.astronomy.angle/rad)))
  (is (= 180.0 (angle/as-value deg-180 :org.soulspace.astronomy.angle/deg)))
  (is (= 12.0  (angle/as-value deg-180 :org.soulspace.astronomy.angle/hour-angle)))
  (is (= m/PI  (angle/as-value deg-540 :org.soulspace.astronomy.angle/rad)))
  (is (= 180.0 (angle/as-value deg-540 :org.soulspace.astronomy.angle/deg)))
  (is (= 12.0  (angle/as-value deg-540 :org.soulspace.astronomy.angle/hour-angle))))

;(run-tests)
