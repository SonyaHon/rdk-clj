(ns sonyahon.rdk-clj.ecs.core
  (:require [clojure.spec.alpha :as s]))

(s/def ::eid uuid?)
(s/def ::entity (s/keys :req [::eid]))
(s/def ::entity-or-id (s/or :eid ::eid
                            :entity ::entity))

(defn- make-world
  "Creates a new empty world"
  []
  {:data {}
   :systems []})
(def ^:private -world (atom (make-world)))

(def ^:private -mut-state (atom {:to-spawn {} :to-destroy #{}}))
(defn- reset-mut-state []
  (reset! -mut-state {:to-spawn {} :to-destroy #{}}))

(defn- craft-entity-data [components]
  (let [eid (java.util.UUID/randomUUID)
        entity (with-meta (apply merge (concat [{::eid eid}] components)) {::entity true})]
    entity))

(defn- destroy-marked [entities]
  (println "Entities: " entities "\nTo destroy: " (:to-destroy @-mut-state))
  (into {} (filter (fn [[id _]]
                     (not ((:to-destroy @-mut-state) id)))
                   entities)))
(defn- spawn-marked [entities]
  (merge entities (:to-spawn @-mut-state)))

(defn- apply-system [system entity]
  (let [result (system entity)]
    (if #_{:clj-kondo/ignore [:unresolved-symbol]}
     (entity? result)
      result
      entity)))

(defn- apply-system-batch-in-parallel
  [entities systems]
  (into {} (->> entities
                (map (fn [entity]
                       (future (let [nentity (reduce (fn [ent sys]
                                                       (apply-system sys ent))
                                                     entity
                                                     systems)]
                                 [(::eid nentity) nentity]))))
                (map #(deref %)))))

(defn- apply-system-batch
  [entities systems]
  (into {} (for [entity entities]
             (let [nentity (reduce (fn [ent sys]
                                     (apply-system sys ent))
                                   entity
                                   systems)]
               [(::eid nentity) nentity]))))

(defn- apply-sections [sections world]
  (loop [sections sections
         world-after world]
    (let [section (first sections)]

      (if (nil? section)
        (let [result (assoc world-after :data (-> (:data world-after)
                                                  (destroy-marked)
                                                  (spawn-marked)))]
          (reset-mut-state)
          result)
        (recur (rest sections)
               (let [maybe-meta (second section)
                     systems (if (map? maybe-meta)
                               (rest (rest section))
                               (rest section))
                     ifcond (if (and (map? maybe-meta) (:if maybe-meta))
                              ((:if maybe-meta))
                              true)
                     batch-applier (if (and (map? maybe-meta) (:in-thread maybe-meta))
                                     apply-system-batch
                                     apply-system-batch-in-parallel)]
                 (if ifcond
                   (let [changed-entities (batch-applier (into [] (vals (:data world-after)))
                                                         systems)]
                     (assoc world-after
                            :data changed-entities))
                   world-after)))))))

;; Direct state manipulation
(defn reset-world*
  "Resets the world state. E.g. deletes all entities and  systems passed. 
   Modifies internal state directly."
  []
  (reset-mut-state)
  (reset! -world (make-world)))

(defn spawn*
  "Spawns a new entity, with attached set of components. 
   Returns the created entity id. Modifies internal state directly."
  [& components]
  (let [{data :data} @-world
        entity-data (craft-entity-data components)]
    (swap! -world
           assoc
           :data (conj data [(::eid entity-data) entity-data]))
    (::eid entity-data)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn destroy*
  "Destroys passed entity. 
   `enity` could be either an actual entity or just an entity id. 
   Modifies internal state directly."
  [entity]
  (let [data (s/conform ::entity-or-id entity)
        eid (condp = (first data)
              :eid (second data)
              :entity (::eid (second data)))
        {data :data} @-world]
    (swap! -world
           assoc
           :data (dissoc data eid))))

(defn set-systems*
  "Set world systems. Accepts syntax kind of simmilar to hiccup
   [:section system-a system-b] <- system-a system-b will be called in parallel
   [:section system-c system-d] <- system-c system-d will be called in parallel after the first section
   [:section {:if cond-function } system-e system-f] <- system-e system-f will be called in parallel after all previous sections and if cond-function will return true
   [:section {:in-thread true} system-g] <- system-g will be run in the main thread"
  [& systems]
  (swap! -world
         assoc
         :systems systems))

(defn apply-systems*
  "Applies systems of the world to it's current data."
  []
  (reset! -world (apply-sections (:systems @-world) @-world)))

;; Utility functions
(defn entity?
  "Checks if object is an entity"
  [obj]
  (::entity (meta obj)))

(defn has
  "Checks if entity has passed components"
  [entity & components]
  (every? entity components))

(defn has-not
  "Checks if entity does not have passed components"
  [entity & components]
  (every? #(not (entity %)) components))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn query
  "Queries world for entities that conform to specified rules.
   Accepts syntax simmilar to hiccup
   [:with ::component-a ::component-b] <- only entities that have both ::component-a and ::component-b will be queried
   [:without ::component-c] <- only entities that do not have ::component-c will be queried"
  [& rules]
  (let [with-rules (->> rules
                        (filter #(= :with (first %)))
                        (map #(into [] (rest %)))
                        (apply concat))
        without-rules (->> rules
                           (filter #(= :without (first %)))
                           (map #(into [] (rest %)))
                           (apply concat))
        entities (vals (:data @-world))]
    (into [] (filter (fn [entity]
                       (and (apply has entity with-rules)
                            (apply has-not entity without-rules))) entities))))

;; Internal system functions

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn destroy!
  "Marks passed entity as one to be deleted in the current apply-systems* call. 
   Does not modify internal world state directly, suggested to be used in systems
   in order to destroy an entity."
  [entity]
  (swap! -mut-state assoc :to-destroy (conj (:to-destroy @-mut-state) (::eid entity))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn spawn!
  "Schedules the creation of passed entity. The entity will be spawned in the end of apply-systems* call.
   Does not modify internal world state directly, suggested to be used in systems 
   in order to spawn an entity"
  [& components]
  (let [entity (craft-entity-data components)
        eid (::eid entity)]
    (swap! -mut-state assoc :to-spawn (conj (:to-spawn @-mut-state) [eid entity]))
    eid))

(comment

  (reset-world*)

  (defn system-fib [entity]
    (when (has entity ::fib)
      (Thread/sleep 200)
      (assoc entity ::fib-res 42)))

  (set-systems* [:section system-fib])

  (doseq [index (range 10)]
    (spawn* {::fib true
             ::index index}))

  (time (apply-systems*))

  42)