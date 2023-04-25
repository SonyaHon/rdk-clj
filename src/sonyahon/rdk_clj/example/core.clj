(ns sonyahon.rdk-clj.example.core
  (:require [sonyahon.rdk-clj.example.components.core :as components]
            [sonyahon.rdk-clj.example.resources.core :as resources]
            [sonyahon.rdk-clj.ecs.core :as ecs]
            [sonyahon.rdk-clj.gfx.canvas :as canva]))

(def app (atom nil))


(defn sys:render [entity]
  (when (ecs/has entity components/position components/renderable)
    (let [{{x :x y :y} components/position
           {ch :ch fg :fg bg :bg} components/renderable} entity]
      (println "Right there!")
      (canva/replace-char! x y ch fg bg))))

(defn sys:apply-user-input [entity]
  (when (ecs/has entity components/position components/player)
    (let [user-input (ecs/get-resource resources/user-input)
          {x :x y :y} (components/position entity)]
      (condp = user-input
        :move-top (assoc entity components/position {:x x :y (dec y)})
        :move-bottom (assoc entity components/position {:x x :y (inc y)})
        :move-left (assoc entity components/position {:x (dec x) :y y})
        :move-right (assoc entity components/position {:x (inc x) :y y})
        nil))))

(defn start-app []
  (canva/intialize-canvas* 100 50)

  (ecs/reset-world*)
  (ecs/set-systems*
   [:section sys:apply-user-input]
   [:section sys:render])

  (ecs/spawn* (components/make-player)
              (components/make-position 10 10)
              (components/make-renderable \@ "#aaff00" "#00000000"))

  (ecs/apply-systems*)
  (canva/render*)

  (reset! app (doto (javax.swing.JFrame. "Example")
                (.add (canva/get-panel))
                (.addKeyListener (proxy [java.awt.event.KeyListener] []
                                   (keyTyped [e] nil)
                                   (keyReleased [e] nil)
                                   (keyPressed [e]
                                     (time (canva/clear!))
                                     (time (ecs/set-resource* resources/user-input :move-right))
                                     (time (ecs/apply-systems*))
                                     (time (canva/render*)))))
                (.setFocusable true)
                (.pack)
                (.setVisible true))))

(defn restart-app []
  (.dispose @app)
  (start-app))

(comment

  42)
