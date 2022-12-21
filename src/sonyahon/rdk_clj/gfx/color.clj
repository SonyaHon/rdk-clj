(ns sonyahon.rdk-clj.gfx.color
  (:import (java.awt Color)))

(defn color
  "Wrapper arround java.awt.Color uses floats"
  ([r g b] (color r g b 1.0))
  ([r g b a] (Color. ^float r ^float g ^float b ^float a)))

