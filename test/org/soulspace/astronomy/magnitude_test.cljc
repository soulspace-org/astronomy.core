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

(ns org.soulspace.astronomy.magnitude-test
  (:require [clojure.test :refer :all]
            [org.soulspace.astronomy.test-utils :as utils]
            [org.soulspace.astronomy.magnitude :refer :all]))

(deftest combined-magnitude-test
  (is (utils/within-error-margin 1.96 (combined-magnitude 1.96) 0.01))
  (is (utils/within-error-margin 1.58 (combined-magnitude 1.96 2.89) 0.01))
  (is (utils/within-error-margin 3.93 (combined-magnitude 4.73 5.22 5.60) 0.01)))

(deftest brightness-ratio-test
  (is (utils/within-error-margin 6.19 (brightness-ratio 0.14 2.12) 0.01)))

