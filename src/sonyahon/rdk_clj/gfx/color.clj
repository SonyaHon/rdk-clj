(ns sonyahon.rdk-clj.gfx.color
  (:import (java.awt Color)))

(defn color
  "Wrapper arround java.awt.Color uses floats"
  ([r g b] (color r g b 1.0))
  ([r g b a] (Color. ^float r ^float g ^float b ^float a)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn darken
  ([col] (.darker col))
  ([col times]
   (reduce (fn [res _]
             (darken res))
           col
           (range times))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn brighten
  ([col] (.brighter col))
  ([col times]
   (reduce (fn [res _]
             (brighten res))
           col
           (range times))))


#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def white (color 1.0 1.0 1.0))
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def red (color 1.0 0.0 0.0))
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def green (color 0.0 1.0 0.0))
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def blue (color 0.0 0.0 1.0))
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def black (color 0.0 0.0 0.0))
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def transparent (color 0.0 0.0 0.0 0.0))