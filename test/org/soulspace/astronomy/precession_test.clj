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

(ns org.soulspace.astronomy.precession-test
  (:require [clojure.test :refer :all]
            [org.soulspace.astronomy.test-utils :as utils]
            [org.soulspace.astronomy.angle :as a]
            [org.soulspace.astronomy.precession :refer :all]))

(deftest calc-m-test
  (is (utils/within-error-margin 3.075 (calc-m 0) 0.001)))

(deftest calc-n-test
  (is (utils/within-error-margin 1.336 (calc-n 0) 0.001)))

; coordinates of Regulus
(def ap-low (annual-precession-low-accuracy 0 [(a/hms-to-rad "10h08m22.3s")
                                           (a/dms-to-rad "11°58'02\"")]))

(def ap-high (annual-precession 0 [(a/hms-to-rad "10h08m22.3s")
                                           (a/dms-to-rad "11°58'02\"")]))

(deftest annual-precession-low-accuracy-test
  (is (utils/within-error-margin   3.208 (first ap-low) 0.001)
      (utils/within-error-margin -17.71  (second ap-low) 0.01)))

(deftest annual-precession-high-accuracy-test
  (is (utils/within-error-margin   3.208 (first ap-high) 0.001)
      (utils/within-error-margin -17.71  (second ap-high) 0.01)))


