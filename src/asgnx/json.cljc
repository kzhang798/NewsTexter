(ns asgnx.json
  #?(:clj (:require [cheshire.core :as cheshire])))

#?(:clj  (defn read-json [s] (cheshire/parse-string s))
   :cljs (defn read-json [s] (js->clj (.parse js/JSON s))))

#?(:clj  (defn write-json [d] (cheshire/generate-string d))
   :cljs (defn write-json [d] (.stringify js/JSON (clj->js d))))


;; Examples:
;;
;; (read-json "{\"a\":1}")
;; (write-json {:a 1})

