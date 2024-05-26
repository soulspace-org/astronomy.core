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

(ns org.soulspace.astronomy.angle
  "Angle functions and abstractions."
  (:require [clojure.spec.alpha :as s]
            [clojure.math :as m]
            [org.soulspace.math.core :as mc]))

; pattern for parsing an angle string given in signed degrees, minutes and seconds, e.g. -80° 7' 30\"
(def dms-pattern #"(\+|-)?(\d+)°\s*(?:(\d+)'\s*(?:(\d+(?:\.\d+)?)\")?)?")
; pattern for parsing a hour angle string given in hours, minutes and seconds, e.g. 10h 7m 30s
(def hms-pattern #"(\d+)h\s*(?:(\d+)m\s*(?:(\d+(?:\.\d+)?)s)?)?")

(comment
  (re-matches dms-pattern "+180° 15'")
  )

(defn hms-to-ha
  "Converts an hour angle given in hours minutes and seconds
   into a hour angle given in decimal hours."
  ([hms]
   (cond
     (map? hms)
     (hms-to-ha {:h hms} {:min hms} {:sec hms})
     (string? hms)
     (let [[_ h min sec] (re-matches hms-pattern hms)
           ;_ (println h min sec)
           hour    (if (seq h) (parse-long h) 0)
           minutes (if (seq min) (parse-long min) 0)
           seconds (if (seq sec) (parse-double sec) 0.0)]
       (hms-to-ha hour minutes seconds))))
  ([h min]
   (hms-to-ha h min 0.0))
  ([h min sec]
   ;(println h min sec)
   (+ h (/ min 60) (/ sec 3600.0))))

(defn ha-to-hms
  "Converts an hour angle given in decimal hours
   in an hour angle in hours, minutes and seconds."
  [ha]
  (let [h (long (m/floor ha))
        hf (rem ha 1)
        m (long (m/floor (* hf 60)))
        mf (rem (* hf 60) 1)]
    {:h h
     :min m
     :sec (double (* mf 60.0))}))

(defn hms-string
  "Returns the string representation of the hour angle."
  [h]
  (if (map? h)
    (str (:h h) "h" (:min h) "m" (:sec h) "s")
    (hms-string (ha-to-hms h))))

(defn ha-to-deg
  "Converts an hour angle to an angle in degrees."
  [ha]
  (* 15 ha))

(defn deg-to-ha
  "Converts an angle in degrees to an hour angle."
  [a]
  (/ (mod a 360) 15))

(defn hms-to-deg
  "Converts an hour angle given in hours, minutes and seconds
   into an angle given in decimal degrees."
  [hms]
  (-> hms
      hms-to-ha
      ha-to-deg))

(defn hms-to-rad
  "Converts an hour angle given in hours, minutes and seconds
   into an angle given in radians."
  [hms]
  (-> hms
      hms-to-ha
      ha-to-deg
      mc/deg-to-rad))

(defn rad-to-hms
  "Converts an angle given in radians into an hour angle
   given in hours, minutes and seconds."
  [a]
  (-> a
      mc/rad-to-deg
      deg-to-ha
      ha-to-hms))

(defn dms-to-deg
  "Converts an angle given in degrees, minutes and seconds
   into an angle given in decimal degrees."
  ([dms]
   (cond
     (map? dms)
     (dms-to-deg (:sign dms) (:deg dms) (:min dms) (:sec dms))
     (string? dms)
     (let [[_ sgn deg min sec] (re-matches dms-pattern dms)
           sgn (if (nil? sgn) "+" sgn)
           deg (if (nil? deg) "0" deg)
           min (if (nil? min) "0" min)
           sec (if (nil? sec) "0" sec)]
       (dms-to-deg (if (= sgn "-") -1 1)
                   (parse-long deg)
                   (parse-long min)
                   (parse-double sec)))))
  ([sgn deg min]
   (dms-to-deg sgn deg min 0.0))
  ([sgn deg min sec]
   (* sgn (+ deg (/ min 60) (/ sec 3600.0)))))

(defn deg-to-dms
  "Converts an angle given in decimal degrees into an angle
   given in degrees, minutes and seconds."
  [a]
  (let [abs-a (abs a)
        af (rem abs-a 1)
        mf (rem (* af 60) 1)]
    {:sign (if (< a 0) -1 1)
     :deg (long (m/floor abs-a))
     :min (long (m/floor (* af 60)))
     :sec (* mf 60.0)}))

(defn dms-to-rad
  "Converts an angle given in degrees, minutes and seconds
   into an angle given in radians."
  [dms]
  (mc/deg-to-rad (dms-to-deg dms)))

(defn rad-to-dms
  "Converts an angle given in radians to an angle
   given in degrees, minutes and seconds."
  [a]
  (deg-to-dms (mc/rad-to-deg a)))

(defn dms-string
  "Returns the string representation of the hour angle."
  [a]
  (if (map? a)
    (str (when (= (:sign a) -1) "-") (:deg a) "°" (:min a) "'" (:sec a) "\"")
    (dms-string (deg-to-dms a))))


;; TODO add specs, add modulo 360, 2*pi, 24 on respective constructors
;; TODO add angle operations: +, - (*, /)?

(s/def ::angle-units #{::rad ::deg ::arcmin ::arcsec ::hour-angle ::dms ::hms})
(def angle-units #{::rad ::deg ::arcmin ::arcsec ::hour-angle ::dms ::hms})
(declare convert-angle)

(defprotocol IAngle
  "Protocol for Angles."
  (as-unit       [this unit] "Returns the angle in the given unit.")
  (as-value      [this unit] "Returns the value of the angle in the given unit.")
  (normalize     [this]      "Returns the angle in the range of [0°-360°[ degrees. Keeps the unit.")
  (as-dms-string [this]      "Returns a string representation as degrees, minutes and seconds.")
  (as-hms-string [this]      "Returns a string representation as an hour angle with minutes and seconds."))

(defrecord Angle [value unit]
  IAngle
  (as-unit       [this unit] (convert-angle this unit))
  (as-value      [this unit] (:value (convert-angle this unit)))
  (normalize     [this]      nil) ; implement modulo 360°
  (as-dms-string [this]      (dms-string (convert-angle this ::dms)))
  (as-hms-string [this]      (hms-string (convert-angle this ::hms)))
  )

(defn create-angle
  "Creates an angle record."
  [value unit]
  (->Angle value unit))

(defmulti convert-angle
  "Converts an angle 'a' to the given unit 'u'."
  (fn [a u] [(:unit a) u]))

(defmethod convert-angle [::rad ::rad] [angle _]
  angle)

(defmethod convert-angle [::deg ::deg] [angle _]
  angle)

(defmethod convert-angle [::arcmin ::arcmin] [angle _]
  angle)

(defmethod convert-angle [::arcsec ::arcsec] [angle _]
  angle)

(defmethod convert-angle [::hour-angle ::hour-angle] [angle _]
  angle)

(defmethod convert-angle [::dms ::dms] [angle _]
  angle)

(defmethod convert-angle [::hms ::hms] [angle _]
  angle)

(defmethod convert-angle [::rad ::deg] [angle _]
  (->Angle (mc/rad-to-deg (:value angle)) ::deg))

(defmethod convert-angle [::deg ::rad] [angle _]
  (->Angle (mc/deg-to-rad (:value angle)) ::rad))

(defmethod convert-angle [::rad ::arcmin] [angle _]
  (->Angle (* (mc/rad-to-deg (:value angle)) 60) ::arcmin))

(defmethod convert-angle [::arcmin ::rad] [angle _]
  (->Angle (mc/deg-to-rad (/ (:value angle) 60)) ::rad))

(defmethod convert-angle [::rad ::arcsec] [angle _]
  (->Angle (* (mc/rad-to-deg (:value angle)) 3600) ::arcsec))

(defmethod convert-angle [::arcsec ::rad] [angle _]
  (->Angle (mc/deg-to-rad (/ (:value angle) 3600)) ::rad))

(defmethod convert-angle [::rad ::hour-angle] [angle _]
  (->Angle (/ (mc/rad-to-deg (:value angle)) 15) ::hour-angle))

(defmethod convert-angle [::hour-angle ::rad] [angle _]
  (->Angle (mc/deg-to-rad (* (:value angle) 15)) ::rad))

(defmethod convert-angle [::deg ::arcmin] [angle _]
  (->Angle (* (:value angle) 60) ::arcmin))

(defmethod convert-angle [::arcmin ::deg] [angle _]
  (->Angle (/ (:value angle) 60) ::deg))

(defmethod convert-angle [::deg ::arcsec] [angle _]
  (->Angle (* (:value angle) 3600) ::arcsec))

(defmethod convert-angle [::arcsec ::deg] [angle _]
  (->Angle (/ (:value angle) 3600) ::deg))

(defmethod convert-angle [::deg ::hour-angle] [angle _]
  (->Angle (/ (:value angle) 15) ::hour-angle))

(defmethod convert-angle [::hour-angle ::deg] [angle _]
  (->Angle (* (:value angle) 15) ::deg))

(defmethod convert-angle [::arcmin ::arcsec] [angle _]
  (->Angle (* (:value angle) 60) ::arcsec))

(defmethod convert-angle [::arcsec ::arcmin] [angle _]
  (->Angle (/ (:value angle) 60) ::arcmin))

(defmethod convert-angle [::arcmin ::hour-angle] [angle _]
  (->Angle (/ (:value angle) 60 15) ::hour-angle))

(defmethod convert-angle [::hour-angle ::arcmin] [angle _]
  (->Angle (* (:value angle) 15 60) ::arcmin))

(defmethod convert-angle [::arcsec ::hour-angle] [angle _]
  (->Angle (/ (:value angle) 3600 15) ::hour-angle))

(defmethod convert-angle [::hour-angle ::arcsec] [angle _]
  (->Angle (* (:value angle) 15 3600) ::arcsec))
