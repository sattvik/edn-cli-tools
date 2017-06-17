(ns io.clojure.edn-cli-tools.transit2edn
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.cli :as cli]
            [cognitect.transit :as transit]
            [io.clojure.edn-cli-tools.io :as io]
            [io.clojure.edn-cli-tools.reader.json :as json]
            ))

(set! *warn-on-infer* true)

(def ^:private ^js/process process (js/require "process"))

(def ^:private option-spec
  [["-c" "--compact"      "Produce compact output"]
   ["-f" "--format FMT"   "Format for transit output (json or json-verbose)"
    :default "json"
    :parse-fn keyword
    :validate [#{:json :json-verbose} "Transit format must be one of json or json-verbose"]]
   ["-e" "--encoding ENC" "Encoding to use when reading/writing"
    :default "utf8"]
   ["-o" "--output FILE"  "File to which to output transit, defaults to stdout"
    :default "-"]
   ["-h" "--help"         "Show this help"]])

(defn ^:private print-header
  "Prints a header useful for user feedback."
  []
  (println "transit2edn - transit to edn converter")
  (println "Usage: transit2edn [options] [files…]")
  (println))

(defn ^:private show-help
  "Shows usage information for the utility."
  [summary]
  (print-header)
  (println summary))

(defn ^:private show-errors
  "Presents information about errors in using the utility."
  [errors]
  (binding [*print-fn* *print-err-fn*]
    (print-header)
    (doseq [error errors]
      (println error)))
  (.exit process 1))


(defn ^:private transit->edn
  "Reads Transit from in and writes EDN to out."
  [^js/stream.Readable in
   ^js/stream.Writable out
   {:keys [compact encoding format]}]
  (let [json-decoder (io/decode-transform json/read encoding ::nil-sentinel)
        encode (let [reader (transit/reader format)]
                 (fn [json-obj]
                   (let [json-str (.stringify js/JSON json-obj)
                         cljs-obj (transit/read reader json-str)]
                     (if compact
                       (str (pr-str cljs-obj) " ")
                       (with-out-str
                         (pprint cljs-obj))))))
        edn-encoder (io/encode-transform encode encoding ::nil-sentinel)]
    (.pipe in json-decoder)
    (.pipe json-decoder edn-encoder)
    (.pipe edn-encoder out)
    (.on json-decoder "error" (fn [^js/Error e] (println "Got an error:" (.-message e))))))

(defn main
  [args]
  (let [{:keys [arguments errors options summary]} (cli/parse-opts args option-spec :strict true)
        {:keys [output]} options]
    (cond
      errors
      (show-errors errors)

      (:help options)
      (show-help summary)

      :default
      (let [out(io/write-stream (or output "-"))
            in-paths (if (empty? arguments) ["-"] arguments)
            inputs (map io/read-stream in-paths)]
        (doseq [in inputs]
          (transit->edn in out options))))))
