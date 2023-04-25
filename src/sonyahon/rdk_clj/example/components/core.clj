(ns sonyahon.rdk-clj.example.components.core)

(def position ::position)
(defn make-position [x y]
  {position {:x x :y y}})

(def renderable ::renderable)
(defn make-renderable [ch fg bg]
  {renderable {:ch ch :fg fg :bg bg}})

(def player ::player)
(defn make-player []
  {player true})