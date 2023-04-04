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

(ns org.soulspace.astronomy.distance-test
  (:require [clojure.test :refer :all]
        [org.soulspace.astronomy.test-utils :as utils]
        [org.soulspace.astronomy.distance :as d])
  ;(:import [org.soulspace.clj.astronomy.distance AstronomicalUnits])
  )

(deftest conversion-test
  (testing "AstronomicalUnit conversions"
    (are [x y] (== x y)
      149597870700 (d/as-value (d/->Distance 1 ::d/au) ::d/m))))
