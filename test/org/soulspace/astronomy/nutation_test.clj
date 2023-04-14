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

(ns org.soulspace.astronomy.nutation-test
  (:require [clojure.test :refer :all]
            [org.soulspace.astronomy.test-utils :as utils]
            [org.soulspace.astronomy.angle :as angle]
            [org.soulspace.astronomy.time :as time]
            [org.soulspace.astronomy.nutation :refer :all]))

(deftest mean-anomaly-sun-test
  (testing "Testing mean anomaly of the sun" 
    (is (utils/within-error-margin
         -4225.0208 (mean-anomaly-sun (time/julian-centuries 2446895.5)))) 0.0001))

(deftest mean-obliquity-test
  (testing "Testing mean obliquity with low accuracy"
    (is (utils/within-error-margin
         23.440946 (mean-obliquity (time/julian-centuries 2446895.5)))))
  (testing "Testing mean obliquity with low accuracy"
    (is (utils/within-error-margin
         23.440946 (mean-obliquity-high-accuracy (time/julian-centuries 2446895.5))))))

