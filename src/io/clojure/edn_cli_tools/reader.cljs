(ns io.clojure.edn-cli-tools.reader
  (:require [cljs.reader :as reader]))

(defprotocol AppendablePushbackReader
  "Useful extensions to ClojureScript’s PushbackReader protocol for working
  with Node’s transformable streams."
  (append [this s]
    "Appends a string to the reader")
  (flush-reader [this]
    "Flushes the reader of all data that has been consumed.")
  (reset-reader [this]
    "Resets the internal offset to the beginning of the buffer."))

(defn appendable-push-back-reader
  "Creates a new instance of an appendable push-back reader."
  []
  (let [buffer (volatile! "")
        offset (volatile! 0)]
    (reify
      reader/PushbackReader
      (read-char [this]
        (when (< @offset (count @buffer))
          (let [c (.charAt @buffer @offset)]
            (vswap! offset inc)
            c)))

      (unread [this ch]
        (when (some? ch)
          (vswap! offset dec)
          (assert (= ch (.charAt @buffer @offset)))))

      AppendablePushbackReader
      (append [this s]
        (let [new-str (.concat @buffer s)]
          (vreset! buffer new-str)))

      (flush-reader [this]
        (let [new-str (.slice @buffer @offset)]
          (vreset! buffer new-str)
          (vreset! offset 0)))

      (reset-reader [this]
        (vreset! offset 0)))))
