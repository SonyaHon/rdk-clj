(ns sonyahon.rdk-clj.gfx.canvas
  (:import (java.awt Dimension)
           (javax.swing JFrame JPanel))
  (:require [sonyahon.rdk-clj.gfx.color :refer [color] :as colors]
            [sonyahon.rdk-clj.gfx.font :as fonts]))

(def ^:private window (atom nil))
(def ^:private canvas-state (atom nil))
(def ^:private canvas-size (atom nil))

(defn- render-string-with-background [gfx text fg bg x y]
  (let [fm (.getFontMetrics gfx)
        rect (.getStringBounds fm text gfx)]
    (doto gfx
      (.setColor bg)
      (.fillRect x (- y (.getAscent fm)) (int (.getWidth rect)) (int (.getHeight rect)))
      (.setColor fg)
      (.drawString text x y))))

(defn- calculate-font-size [font]
  (let [frame (doto (JFrame.)
                (.setSize (Dimension. 100 100)))
        fm (.getFontMetrics frame font)
        bounds (.getStringBounds fm " " (.getGraphics frame))]
    (println bounds)
    {:width (int (.getWidth bounds)) :height (int (.getHeight bounds))}))

(defn- d1->d2 [idx]
  [(int (mod idx (@canvas-size :width)))
   (int (/ idx (@canvas-size :width)))])

(defn- d2->d1 [x y]
  (+ x (* y (@canvas-size :width))))

(defn- get-initial-state [width height]
  (into [] (repeat (* width height) [{:char \ 
                                      :foreground (color 1.0 1.0 1.0 0.0)
                                      :background (color 0.0 0.0 0.0 1.0)}])))

(defn- draw-char! [char fg bg x y]
  (swap! canvas-state assoc
         (d2->d1 x y)
         (conj (get @canvas-state (d2->d1 x y)) {:char char :foreground fg :background bg})))

(defn- draw-string! [text fg bg x y]
  (let [swapdata (loop [swapdata []
                        cursor x
                        chars text]
                   (if (or (>= cursor (:width @canvas-size))
                           (>= cursor (+ x (count text))))
                     swapdata
                     (recur
                      (concat swapdata [(d2->d1 cursor y) (conj
                                                           (get @canvas-state (d2->d1 y cursor))
                                                           {:char (first chars)
                                                            :foreground fg
                                                            :background bg})])
                      (inc cursor)
                      (rest chars))))]
    (swap! canvas-state
           #(apply assoc % (flatten swapdata)))))

;; State manipulation
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn intialize-canvas*
  "Initializes a new window and canvas"
  ([char-width char-height] (intialize-canvas* char-width char-height fonts/DEFAULT 16))
  ([char-width char-height font font-size]
   (reset! canvas-size {:width char-width :height char-height})
   (reset! canvas-state (get-initial-state char-width char-height))
   (let [font (.deriveFont font {java.awt.font.TextAttribute/SIZE font-size})
         {font-width :width font-height :height} (calculate-font-size font)
         panel (doto (proxy [JPanel] []
                       (paintComponent [gfx]
                         (proxy-super paintComponent gfx)
                         (let [gfx (.create gfx)]
                           (.setFont gfx font)
                           (doseq [[idx chars] (map-indexed vector @canvas-state)]
                             (let [[x y] (d1->d2 idx)]
                               (doseq [{char :char fg :foreground bg :background} chars]
                                 (render-string-with-background gfx
                                                                (str char)
                                                                fg bg
                                                                (* font-width x)
                                                                (* font-height (inc y))))))
                           (.dispose gfx))))
                 (.setPreferredSize (Dimension.
                                     (* char-width font-width)
                                     (* char-height font-height))))]
     (println "Font: " font-width "x" font-height)
     (reset! window {:panel panel
                     :font font}))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn render* []
  (.repaint (:panel @window)))

;; Drawing
(defmulti draw!
  (fn [target & _]
    (class target)))

(defmethod draw! String
  [target & rest]
  (apply draw-string! target rest))
(defmethod draw! Character
  [target & rest]
  (apply draw-char! target rest))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn clear!
  ([]
   (let [{width :width
          height :height} @canvas-size]
     (reset! canvas-state (get-initial-state width height))))
  ([x y width height]
   (let [changes (into [] (flatten (for [cx (range x (+ x width))
                                         cy (range y (+ y height))] [(d2->d1 cx cy) {:char \ 
                                                                                     :foreground (color 1.0 1.0 1.0 1.0)
                                                                                     :background (color 0.0 0.0 0.0 1.0)}])))]
     (swap! canvas-state
            #(apply assoc % changes)))))
