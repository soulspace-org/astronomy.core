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

(ns org.soulspace.astronomy.distance)

;;;
;;; Functions for astronomical distances
;;;

(def ^:const KM_M "Kilometer [m]" 1000)
(def ^:const AU_M "Astronomical Unit [m]" 149597870700)
(def ^:const LY_M "Light year [m]" 9460730472580800)
(def ^:const LY_AU "Light year [au]" 63241.077)
(def ^:const PC_M "Parsec (Parallax second) [m]" 96939420213600000)
(def ^:const PC_LY "Parsec (Parallax second) [ly]" 3.2616)

(def distance-units #{::m ::km ::au ::ly ::pc})

(defn meters-to-astronomical-units
  "Converts the distance given in meters to astronomical units."
  [d]
  (/ d AU_M))

(defn meters-to-light-years
  "Converts the distance given in meters to light years."
  [d]
  (/ d LY_M))

(defn meters-to-parsecs
  "Converts the distance given in meters to parsecs."
  [d]
  (/ d PC_M))

(defn astronomical-units-to-meters
  "Converts the distance given in astronomical units to meters."
  [d]
  (* d AU_M))

(defn astronomical-units-to-light-years
  "Converts the distance given in astronomical units to light years."
  [d]
  (/ d LY_AU))

(defn astronomical-units-to-parsecs
  "Converts the distance given in astronomical units to parsecs."
  [d]
  (/ d LY_AU PC_LY))

(defn light-years-to-meters
  "Converts the distance given in light years to meters."
  [d]
  (* d LY_M))

(defn light-years-to-astronomical-units
  "Converts the distance given in light years to astronomical units."
  [d]
  (* d LY_AU))

(defn light-years-to-parsecs
  "Converts the distance given in light-years-to-parsecs."
  [d]
  (/ d PC_LY))

(defn parsecs-to-meters
  "Converts the distance given in parsecs to light years."
  [d]
  (* d PC_M))

(defn parsecs-to-astronomical-units
  "Converts the distance given in parsecs to astronomical units."
  [d]
  (* d PC_LY LY_AU))

(defn parsecs-to-light-years
  "Converts the distance given in parsecs to light years."
  [d]
  (* d PC_LY))

; TODO move protocols and record in a domain layer

(declare convert-distance)

(defprotocol IDistance
  (as-unit [this unit] "Returns the distance in the given unit.")
  (as-value [this unit] "Returns the value of the distance in the given unit."))

(defrecord Distance [value unit]
  IDistance
  (as-unit [this unit] (convert-distance this unit))
  (as-value [this unit] (:value (convert-distance this unit))))

(defmulti convert-distance
  "Converts the given distance to the unit."
  (fn [dist unit] [(:unit dist) unit]))

(defmethod convert-distance [::m ::m] [dist _]
  dist)

(defmethod convert-distance [::km ::km] [dist _]
  dist)

(defmethod convert-distance [::au ::au] [dist _]
  dist)

(defmethod convert-distance [::ly ::ly] [dist _]
  dist)

(defmethod convert-distance [::pc ::pc] [dist _]
  dist)

(defmethod convert-distance [::m ::km] [dist _]
  (->Distance (/ (:value dist) KM_M) ::km))

(defmethod convert-distance [::km ::m] [dist _]
  (->Distance (* (:value dist) KM_M) ::m))

(defmethod convert-distance [::m ::au] [dist _]
  (->Distance (/ (:value dist) AU_M) ::au))

(defmethod convert-distance [::au ::m] [dist _]
  (->Distance (* (:value dist) AU_M) ::m))

(defmethod convert-distance [::km ::au] [dist _]
  (->Distance (/ (:value dist) AU_M KM_M) ::au))

(defmethod convert-distance [::au ::km] [dist _]
  (->Distance (/ (* (:value dist) AU_M) KM_M) ::km))

(defmethod convert-distance [::m ::ly] [dist _]
  (->Distance (/ (:value dist) LY_M) ::ly))

(defmethod convert-distance [::ly ::m] [dist _]
  (->Distance (* (:value dist) LY_M) ::m))

(defmethod convert-distance [::km ::ly] [dist _]
  (->Distance (/ (:value dist) LY_M KM_M) ::ly))

(defmethod convert-distance [::ly ::km] [dist _]
  (->Distance (/ (* (:value dist) LY_M) KM_M) ::km))

(defmethod convert-distance [::au ::ly] [dist _]
  (->Distance (/ (:value dist) LY_AU) ::ly))

(defmethod convert-distance [::ly ::au] [dist _]
  (->Distance (* (:value dist) LY_AU) ::au))

(defmethod convert-distance [::m ::pc] [dist _]
  (->Distance (/ (:value dist) PC_M) ::pc))

(defmethod convert-distance [::pc ::m] [dist _]
  (->Distance (* (:value dist) PC_M) ::m))

(defmethod convert-distance [::km ::pc] [dist _]
  (->Distance (/ (:value dist) PC_M KM_M) ::pc))

(defmethod convert-distance [::pc ::km] [dist _]
  (->Distance (/ (* (:value dist) PC_M) KM_M) ::km))

(defmethod convert-distance [::au ::pc] [dist _]
  (->Distance (/ (:value dist) PC_LY LY_AU) ::pc))

(defmethod convert-distance [::pc ::au] [dist _]
  (->Distance (* (:value dist) PC_LY LY_AU) ::au))

(defmethod convert-distance [::ly ::pc] [dist _]
  (->Distance (/ (:value dist) PC_LY) ::pc))

(defmethod convert-distance [::pc ::ly] [dist _]
  (->Distance (* (:value dist) PC_LY) ::ly))

(def ONE_AU (->Distance 1 ::au))
(def ONE_LY (->Distance 1 ::ly))
(def ONE_PC (->Distance 1 ::pc))