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

(ns org.soulspace.clj.astronomy.time-test
  (:require [clojure.test :refer :all]
            [org.soulspace.clj.astronomy.test-utils :as atest]
            [org.soulspace.clj.astronomy.angle :refer [deg-to-ha]]
            [org.soulspace.clj.astronomy.time :refer :all]))

(deftest julian-day-test
  (are [x y] (= x y)
    2451545.0 (date-to-julian-day 2000 1 1.5)
    2451179.5 (date-to-julian-day 1999 1 1.0)
    2447187.5 (date-to-julian-day 1988 1 27.0)
    2447332.0 (date-to-julian-day 1988 6 19.5)
    2446822.5 (date-to-julian-day 1987 1 27.0)
    2446966.0 (date-to-julian-day 1987 6 19.5)
    2436116.31 (date-to-julian-day 1957 10 4.81)
    2415020.5 (date-to-julian-day 1900 1 1.0)
    2305447.5 (date-to-julian-day 1600 1 1.0)
    2305812.5 (date-to-julian-day 1600 12 31.0)
    2299159.5 (date-to-julian-day 1582 10 4)
    2299160.5 (date-to-julian-day 1582 10 15)
    2026871.8 (date-to-julian-day 837 4 10.3)
    1842713.0 (date-to-julian-day 332 13 27.5)
    1676496.5 (date-to-julian-day -123 12 31.0)
    1676497.5 (date-to-julian-day -122 1 1.0)
    0.0 (date-to-julian-day -4712 1 1.5)))


(deftest leap-year-test
  (are [x y] (= x y)
    true  (leap-year? 4000)
    false (leap-year? 2100)
    false (leap-year? 2010) 
    true  (leap-year? 2004) 
    true  (leap-year? 2000)
    false (leap-year? 1999)
    true  (leap-year? 1996)
    false (leap-year? 1900)
    false (leap-year? 1899)
    false (leap-year? 1700)
    true  (leap-year? 1600)
    false (leap-year? 1582)
    true  (leap-year? 1500)
    true  (leap-year? 1492)
    true  (leap-year? 0)
    false (leap-year? -1)
    true  (leap-year? -4)
    true  (leap-year? -400)
    true  (leap-year-by-julian-day? 2451545.0)
    false (leap-year-by-julian-day? 2451179.5)
    true  (leap-year-by-julian-day? 2268932.5)))


(deftest day-of-week-test
  (is (= (day-of-week 2434923.5) :wednesday))
  (is (= (day-of-week 2299159.5) :thursday))
  (is (= (day-of-week 2299160.5) :friday)))


(deftest easter-date-gregorian-test
  (is (= (easter-date-by-gregorian-date 1991) {:year 1991 :month 3 :day 31})))


(deftest easter-date-julian-test
  (is (= (easter-date-by-julian-date 179) {:year 179 :month 4 :day 12})))

(deftest day-of-year-test
  (is (== (day-of-year 2299160.5) 288))
  (is (== (day-of-year 1582 10 15) 288)))

(deftest mean-siderial-time-greenwich-0ut-test
  (is (atest/within-error-margin (deg-to-ha (mean-siderial-time-greenwich-0ut 2446895.5)) 13.1795)))

(deftest mean-siderial-time-greenwich-test
  (is (atest/within-error-margin (deg-to-ha (mean-siderial-time-greenwich 2446895.5)) 13.1795)))

;(run-tests)
