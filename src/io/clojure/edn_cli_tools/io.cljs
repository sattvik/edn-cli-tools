(ns io.clojure.edn-cli-tools.io
  (:require [cljs.nodejs :as node]
            [io.clojure.edn-cli-tools.reader :as r]))

(set! *warn-on-infer* true)

(def ^:private fs ^js/fs (js/require "fs"))
(def ^:private process ^js/Process (js/require "process"))
(def ^:private Transform (.-Transform ^js/stream (js/require "stream")))

(defn ^js/stream.Readable read-stream
  "Given a path, return a ReadStream to that path.  A ‘-’ is interpreted as
  stdin."
  [path]
  (if (= "-" path)
    (.-stdin process)
    (.createReadStream fs path)))

(defn ^js/stream.Writable write-stream
  "Given a path, return a WriteStream to that path.  A ‘-’ is intepreted as
  stdout."
  [path]
  (if (= "-" path)
    (.-stdout process)
    (.createWriteStream fs path)))

(defn ^js/stream.Transform decode-transform
  "Creates a Node Transform stream that will read objects from the incoming
  stream and place them onto the outgoing stream as objects.  Nil values will
  be transmitted as the nil-sentinel.  The encoding is used to decode any
  buffers into strings."
  [read-fn encoding nil-sentinel]
  (let [push-back-reader (r/appendable-push-back-reader)]
    (Transform. #js {:transform (fn [chunk chunk-enc callback]
                                  (let [s (if (= chunk-enc "buffer")
                                            (.toString ^js/Buffer chunk encoding)
                                            chunk)]
                                    (r/append push-back-reader s)
                                    (try
                                      (loop [obj (read-fn push-back-reader false ::eof false)]
                                        (when (not= ::eof obj)
                                          (this-as ^js/stream.Readable this (.push this (if (nil? obj)
                                                                                          nil-sentinel
                                                                                          obj)))
                                          (r/flush-reader push-back-reader)
                                          (recur (read-fn push-back-reader false ::eof false))))
                                      (callback nil nil)
                                      (catch js/Error ^js/Error e
                                        (if (= "EOF while reading" (.-message e))
                                          (do
                                            (r/reset-reader push-back-reader)
                                            (callback nil nil))
                                          (callback e nil))))))
                     :final (fn [callback]
                              (try
                                (let [obj (read-fn push-back-reader false ::eof false)]
                                  (if (= obj ::eof)
                                    (callback nil)
                                    (throw (ex-info "WTF?" {:read-object obj}))))
                                (catch js/Error e
                                  (callback e))))
                     :readableObjectMode true
                     :decodeStrings false})))

(defn ^js/stream.Transform encode-transform
  [write-fn encoding nil-sentinel]
  (Transform. #js {:transform (fn [obj _ callback]
                                (let [obj (if (= obj nil-sentinel) nil obj)
                                      s (write-fn obj)]
                                  (this-as ^js/stream.Readable this
                                    (.push this s)))
                                (callback nil nil))
                   :writableObjectMode true
                   :encoding encoding}))

; vim:set lispwords+=this-as:
