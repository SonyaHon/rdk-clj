(ns sonyahon.rdk-clj.gfx.canvas
  (:require clojure.string
            [sonyahon.rdk-clj.gfx.font :as fonts]
            [com.evocomputing.colors :refer [create-color]])
  (:import (java.awt Dimension Color)
           (javax.swing JFrame JPanel)))

(defn- awt-color [color]
  (let [[r g b a] (:rgba color)]
    (Color. r g b a)))

(defn- str->color [str]
  (awt-color (create-color str)))

(def ^:private window (atom nil))
(def ^:private canvas-state (atom nil))
(def ^:private canvas-size (atom nil))

(def ^:private color:transparent "#00000000")
(def ^:private color:white "#ffffff")
(def ^:private color:black "#000000")
(def ^:private canvas-bg-color (atom color:black))


(defn- get-bg-char-data []
  {:char \ 
   :foreground color:transparent
   :background @canvas-bg-color})

(defn- render-string-with-background [gfx text fg bg x y]
  (let [fm (.getFontMetrics gfx)
        rect (.getStringBounds fm text gfx)]
    (doto gfx
      (.setColor (str->color bg))
      (.fillRect x (- y (.getAscent fm)) (int (.getWidth rect)) (int (.getHeight rect)))
      (.setColor (str->color fg))
      (.drawString text x y))))

(defn- calculate-font-size [font]
  (let [frame (doto (JFrame.)
                (.setSize (Dimension. 100 100)))
        fm (.getFontMetrics frame font)
        bounds (.getStringBounds fm " " (.getGraphics frame))]
    {:width (int (.getWidth bounds)) :height (int (.getHeight bounds))}))

(defn- d1->d2 [idx]
  [(int (mod idx (@canvas-size :width)))
   (int (/ idx (@canvas-size :width)))])

(defn- d2->d1 [x y]
  (+ x (* y (@canvas-size :width))))

(defn- get-initial-state [width height]
  (let [clear-char (get-bg-char-data)]
    (into []
          (repeat (* width height) [clear-char]))))

(defn- overflows? [text words max-len]
  (let [last-line (last (clojure.string/split-lines text))
        word (first words)
        count-last-line (count (clojure.string/replace last-line #"(?sm)(?<!%)%(fg|bg)\{([#x\w]+)?}" ""))
        count-word (count (clojure.string/replace word #"(?sm)(?<!%)%(fg|bg)\{([#x\w]+)?}" ""))]
    (> (+ count-last-line count-word) max-len)))

(defn- divide-text [text max-len]
  (let [words (clojure.string/split text #" ")]
    (clojure.string/trim
     (loop [resulting-text ""
            words words]
       (if (empty? words)
         resulting-text
         (let [r-text (if (overflows? resulting-text words max-len)
                        (str resulting-text "\n" (first words))
                        (str resulting-text " " (first words)))
               r-words (rest words)]
           (recur
            r-text
            r-words)))))))

(defn- gen-str-assoc [x just-text cursor current-fg current-bg replace]
  (loop [chars just-text
         cursor cursor
         res []]
    (if-let [char (first chars)]
      (condp = char
        \newline (recur
                  (rest chars)
                  {:x x :y (inc (:y cursor))}
                  res)
        (let [this-char [(d2->d1 (cursor :x) (cursor :y))
                         (if replace
                           [(get-bg-char-data)
                            {:char char
                             :foreground current-fg
                             :background current-bg}]
                           (conj (get @canvas-state (d2->d1 (cursor :x) (cursor :y)))
                                 {:char char
                                  :foreground current-fg
                                  :background current-bg}))]
              next-cursor {:x (inc (:x cursor)) :y (:y cursor)}]
          (recur
           (rest chars)
           next-cursor
           (concat res this-char))))
      [res cursor])))

(defn- draw-string [x y text max-line-length should-replace]
  (let [swapdata (loop [swapdata []
                        current-fg color:white
                        current-bg color:transparent
                        cursor {:x x :y y}
                        data (if (some? max-line-length) (divide-text text max-line-length) text)]
                   (if (empty? data)
                     swapdata
                     (if (re-find #"(?sm)(.*?)(?<!%)%(fg|bg)\{([#x\w]+)?}" data)
                       (let [[parsed just-text fgbg color] (re-find #"(?sm)(.*?)(?<!%)%(fg|bg)\{([#x\w]+)?}" data)
                             r-current-fg (if (and (= fgbg "fg") (some? color)) color color:white)
                             r-current-bg (if (and (= fgbg "bg") (some? color)) color color:transparent)
                             r-data (subs data (count parsed))
                             [r-swapdata r-cursor] (gen-str-assoc x just-text cursor current-fg current-bg should-replace)]
                         (recur
                          (concat swapdata r-swapdata)
                          r-current-fg
                          r-current-bg
                          r-cursor
                          r-data))
                       (let [[r-swapdata r-cursor] (gen-str-assoc x data cursor current-fg current-bg should-replace)]
                         (recur
                          (concat swapdata r-swapdata)
                          current-fg
                          current-bg
                          r-cursor
                          "")))))]
    (swap! canvas-state
           #(apply assoc % swapdata))
    nil))

;; State manipulation
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn intialize-canvas*
  "Initializes a new window and canvas, accepts width and height of the window in characters"
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
                 (.setFocusable true)
                 (.requestFocusInWindow)
                 (.setPreferredSize (Dimension.
                                     (* char-width font-width)
                                     (* char-height font-height))))]
     (reset! window {:panel panel
                     :font font}))
   nil))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn render*
  "Re-renders current state"
  []
  (.repaint (:panel @window)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn set-bg-color*
  "Sets the default background/clear color"
  [color]
  (reset! canvas-bg-color color)
  nil)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn add-char!
  "Adds a character onto whats on the x:y position on the screen.
   If you want to replace whats already on the screen, use replace-char!
   To see this changes one still needs to call `render*`.
   If background is not passed, transparent color will be used.
   If foreground is not passed, white color will be used."
  ([x y char]
   (add-char! x y char color:white color:transparent))
  ([x y char foreground]
   (add-char! x y char foreground color:transparent))
  ([x y char foreground background]
   (swap! canvas-state assoc
          (d2->d1 x y)
          (conj
           (get @canvas-state (d2->d1 x y))
           {:char char
            :foreground foreground
            :background background}))
   nil))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn replace-char!
  "Replaces current x:y position with passed character. 
   If you want to draw on top see `add-char!`
   If you want to replace whats already on the screen, use replace-char!
   To see this changes one still needs to call `render*`.
   If background is not passed, transparent color will be used.
   If foreground is not passed, white color will be used."
  ([x y char]
   (replace-char! x y char color:white color:transparent))
  ([x y char foreground]
   (replace-char! x y char foreground color:transparent))
  ([x y char foreground background]
   (swap! canvas-state assoc
          (d2->d1 x y)
          [(get-bg-char-data)
           {:char char
            :foreground foreground
            :background background}])
   nil))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn add-text!
  "Adds a string starting from x y on top of whats already there.
   Line breaks are preserved. If max-len passed, than the string is
   split at word boundaries with newlines.
   
   To add color to the passed string use %fg{#0055ff} to set foreground
   and %bg{#0055ff} to set background colors respectevly. If no color passed
   the defaults of white for fg and transparent for bg are used.
   
   If you are interested in replacing current context see `replace-text!`
   If you are interested in string formatting see `format`"
  ([x y text] (add-text! x y text nil))
  ([x y text max-line-length]
   (draw-string x y text max-line-length false)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn replace-text!
  "Replaces current context starting from x y with the contents of the string.
   Line breaks are preserved. If max-len passed, than the string is
   split at word boundaries with newlines.
   
   To add color to the passed string use %fg{#0055ff} to set foreground
   and %bg{#0055ff} to set background colors respectevly. If no color passed
   the defaults of white for fg and transparent for bg are used.
   
   If you are interested in replacing current context see `replace-text!`
   If you are interested in string formatting see `format`"
  ([x y text] (replace-text! x y text nil))
  ([x y text max-line-length]
   (draw-string x y text max-line-length true)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn clear!
  ([]
   (let [{width :width
          height :height} @canvas-size]
     (reset! canvas-state (get-initial-state width height))
     nil))
  ([x y width height]
   (let [bg-char (get-bg-char-data)
         changes (->> (for [cx (range x (+ x width))
                            cy (range y (+ y height))] [(d2->d1 cx cy) bg-char])
                      (flatten)
                      (into []))]
     (swap! canvas-state
            #(apply assoc % changes))
     nil)))