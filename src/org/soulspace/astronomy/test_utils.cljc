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

(ns org.soulspace.astronomy.test-utils)

(defn within-error-margin
  "Tests if the actual and expected values are equal in the given error margin."
  ([actual expected]
   (within-error-margin actual expected 0.0001))
  ([actual expected error-margin]
   (<= (abs (- actual expected)) error-margin)))
