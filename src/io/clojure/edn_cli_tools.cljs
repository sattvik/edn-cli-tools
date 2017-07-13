(ns io.clojure.edn-cli-tools
  (:require [clojure.string :as str]
            [cljs.nodejs :as nodejs]
            [io.clojure.edn-cli-tools.edn2transit :as e2t]
            [io.clojure.edn-cli-tools.transit2edn :as t2e]))

(set! *warn-on-infer* true)
(nodejs/enable-util-print!)

(defn invoke-type
  []
  (let [process ^js/Process (nodejs/require "process")
        argv (.-argv process)
        invoked-as (aget argv 1)]
    (cond
      (str/ends-with? invoked-as "edn2transit") :edn->transit
      (str/ends-with? invoked-as "transit2edn") :transit->edn)))

(defn main
  [& args]
  (case (or (invoke-type) (first args))
    :edn->transit (e2t/main args)
    :transit->edn (t2e/main args)
    "edn2transit" (e2t/main (rest args))
    "transit2edn" (t2e/main (rest args))))

(set! *main-cli-fn* main)
