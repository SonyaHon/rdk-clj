(ns sonyahon.rdk-clj.gfx.font
  (:import (java.awt Font))
  (:require [clojure.java.io :as io]))

(defn create-font
  "Creates a new font from file a .ttf file"
  [font-path]
  (let [font-file (io/file font-path)]
    (Font/createFont Font/TRUETYPE_FONT ^java.io.File font-file)))

(def DEFAULT (create-font "resources/default.ttf"))