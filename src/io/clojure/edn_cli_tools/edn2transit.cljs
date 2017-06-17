(ns io.clojure.edn-cli-tools.edn2transit
  (:require [clojure.tools.cli :as cli]
            [cljs.reader :as reader]
            [cognitect.transit :as transit]
            [io.clojure.edn-cli-tools.io :as io]))

(set! *warn-on-infer* true)

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
  (println "edn2transit - edn to transit converter")
  (println "Usage: edn2transit [options] [files…]")
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
  (.exit ^js/process (js/require "process") 1))


(defn ^:private edn->transit
  "Reads EDN forms from in and writes transit to out."
  [^js/stream.Readable in
   ^js/stream.Writable out
   {:keys [compact encoding format]}]
  (let [edn-decoder (io/decode-transform reader/read encoding ::nil-sentinel)
        encode (let [writer (transit/writer format)]
                 (fn [obj]
                   (let [s (transit/write writer obj)]
                     (if compact s (str s "\n")))))
        transit-encoder (io/encode-transform encode encoding ::nil-sentinel)]
    (.pipe in edn-decoder)
    (.pipe edn-decoder transit-encoder)
    (.pipe transit-encoder out)
    (.on edn-decoder "error" (fn [^js/Error e] (println "Got an error:" (.-message e))))))

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
          (edn->transit in out options))))))
