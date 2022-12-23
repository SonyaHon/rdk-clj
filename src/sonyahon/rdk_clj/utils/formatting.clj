(ns sonyahon.rdk-clj.utils.formatting
  (:require [clojure.string :refer [split replace-first]]))

(def ^:private saved-format-symbols (atom {}))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn add-format-specifier*
  "Registers a new specifier to be used with `format`
   * `specifier` is just a string, name of specifier
   * `handler` must be a function of variadic arguments where the first 1
               is a substituin passed and all the others are the parameters
               passed in {}"
  [specifier handler]
  (swap! saved-format-symbols assoc specifier handler)
  nil)

#_{:clj-kondo/ignore [:redefined-var :clojure-lsp/unused-public-var]}
(defn format
  "Formats passed string with custom substituitions and specifiers.
   If a specifier is not registered, defaults to `clojure.core/format`
   
   All custom specifiers have a form of %REGISTERED_NAME{[OPTIONAL_ARGS]}
   e.g. %name{} %to-case{uppercase}
   To register a specifier see `add-format-specifier`
   
   Example:
   (format \"This is a %name{}\" {:name \"Goblin\"}) ;; This is a Goblin"
  [text & subs]
  (loop [data {:text text :subs subs}
         matches (re-seq #"(?<!%)%(?<specifier>[- ,(.+\w]*\w)(?<args>\{[\w,]*\})?" text)]
    (println matches)
    (let [match (first matches)]
      (if (nil? match)
        (:text data)
        (recur
         (if (#{"fg" "bg"} (second match))
           data
           (let [[t specifier args] match
                 args (if (nil? args) [] (-> (clojure.core/subs args 1 (dec (count args)))
                                             (split #",")))
                 handler (or (@saved-format-symbols specifier) (fn [& args]
                                                                 (apply clojure.core/format t args)))]
             {:text (replace-first (:text data) t (apply handler (first subs) args))
              :subs (rest subs)}))
         (rest matches))))))