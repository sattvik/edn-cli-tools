(ns io.clojure.edn-cli-tools.io-test
  (:require [clojure.test :refer [async deftest is]]
            [cljs.reader :as edn]
            [io.clojure.edn-cli-tools.io :as io]
            [io.clojure.edn-cli-tools.reader.json :as json]))


(def stream (js/require "stream"))
(def Readable (.-Readable stream))
(def Writable (.-Writable stream))

(defn make-str-readable
  "Creates a Readable with the given string as the contents of the stream."
  [s]
  (Readable. #js {:read (fn [_]
                          (this-as this
                            (.push this (.from js/Buffer s "utf8"))
                            (.push this nil)))
                  #_#_:encoding "utf8"}))

(defn make-obj-readable
  "Creates a Readable that will produce the given objects in a stream."
  [objs]
  (let [obj-seq (volatile! (seq objs))]
    (Readable. #js {:read (fn [_]
                            (this-as this
                              (while (.push this (first @obj-seq))
                                (vswap! obj-seq next))))
                    :objectMode true })))

(defn make-writable
  []
  (let [data (atom [])
        writer (Writable. #js {:write (fn [datum _ cb]
                                        (swap! data conj datum)
                                        (cb nil))
                               :objectMode true })]
    [data writer]))

(deftest test-decode-edn
  (async done
    (let [in (make-str-readable "[] {} :foo nil \"a\"")
          xform (io/decode-transform edn/read "utf8" ::nil)
          [data out] (make-writable)]
      (.on out "finish" (fn []
                          (is (= [[] {} :foo ::nil "a"]
                                 @data))
                          (done)))
      (-> in
          (.pipe xform)
          (.pipe out)))))

(deftest test-decode-json
  (async done
    (let [in (make-str-readable "[1, true, false] {\"foo\": null}")
          xform (io/decode-transform json/read "utf8" ::nil)
          [data out] (make-writable)]
      (.on out "finish" (fn []
                          (is (= [[1 true false] {"foo" nil}]
                                 (js->clj @data)))
                          (done)))
      (-> in
          (.pipe xform)
          (.pipe out)))))

(deftest test-encode-edn
  (let [in (make-obj-readable [[] {:foo 1} ::nil "42"])
        xform (io/encode-transform pr-str "utf8" ::nil)
        [data out] (make-writable)]
    (async done
      (.on out "finish" (fn []
                          (is (= ["[]" "{:foo 1}" "nil" "\"42\""]
                                 @data))
                          (done)))
      (-> in
          (.pipe xform)
          (.pipe out)))))

(deftest test-encode-json
  (let [in (make-obj-readable [#js [1 true false "42"] #js {:foo 1 :bar nil} ::nil])
        xform (io/encode-transform #(.stringify js/JSON %) "utf8" ::nil)
        [data out] (make-writable)]
    (async done
      (.on out "finish" (fn []
                          (is (= ["[1,true,false,\"42\"]" "{\"foo\":1,\"bar\":null}" "null"]
                                 @data))
                          (done)))
      (-> in
          (.pipe xform)
          (.pipe out)))))

; vim:set lispwords+=async,this-as:
