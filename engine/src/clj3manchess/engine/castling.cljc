(ns clj3manchess.engine.castling
  (:require [schema.core :as s]
            #?(:clj [clojure.spec :as sc]
               :cljs [cljs.spec :as sc])
            [clj3manchess.engine.color :as col :refer [Color]]
            [clojure.set :as set]
            [clojure.string :as str]
            [clj3manchess.engine.color :as c]))

(def types #{:queenside :kingside})
(sc/def ::type types)
(sc/def ::castling ::type)
(def CastlingType (s/enum :queenside :kingside))

(def CastlingPossibility {(s/required-key :color) Color
                          (s/required-key :type) CastlingType})
(sc/def ::possibility (sc/keys :req-un [::col/color ::type]))

;;(def ColorCastlingPossibilities {CastlingType})

;;(def CastlingPossibilities {Color ColorCastlingPossibilities})
(def CastlingPossibilities #{CastlingPossibility})
(sc/def ::possibilities (sc/coll-of ::possibility :kind set? :min-count 0 :max-count 6 :distinct true :into #{}))
(def castchar {:queenside \q :kingside \k})
(def charcast (set/map-invert castchar))
(def allcastpossib (set/join (set (map #(assoc {} :color %) col/colors))
                             (set (map #(assoc {} :type %) #{:queenside :kingside}))))
(defn castposchar [{:keys [color type]}] (str (col/colchar color) (castchar type)))
(def castposcharmap (->> allcastpossib
                         (map #(vec [% (castposchar %)]))
                         vec
                         (into {})))
(def charcastpos (set/map-invert castposcharmap))
(defn charsetcastposset [st] (->> st
                                  (partition-by #{\,})
                                  (filter #(not= \, (first %)))
                                  (map str/join)
                                  (map charcastpos)))
;;(charsetcastposset (str/join \, (keys charcastpos)))
(defn castpossetstr [castposset] (->> castposset
                                      (map castposchar)
                                      (str/join \,)))
;;(castpossetstr (vals charcastpos))
