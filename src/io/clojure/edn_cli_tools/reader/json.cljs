(ns io.clojure.edn-cli-tools.reader.json
  "Provides a JSON parser that works with ClojureScript’s PushbackReader.
  Also, provides a `read` function that has an interface similar to
  `cljs.reader/read`, but reads JSON and produces JavaScript objects."
  {:author "Daniel Solano Gómez"}
  (:require [clojure.set :as set]
            [cljs.reader :as r]))

(def ^:private is-whitespace?
  "As defined by ECMA-404"
  #{\u0009 ; character tabulation
    \u000a ; line feed
    \u000d ; carriage return
    \u0020 ; space
    })

(defn ^:private consume-whitespace!
  "Consumes all beginning whitespace from the reader.  Returns the whitespace
  that was read."
  [in]
  (loop [c (r/read-char in)
         context ""]
    (cond
      ;; end of input
      (nil? c)
      context

      ;; found whitespace
      (is-whitespace? c)
      (recur (r/read-char in)
             (str context c))

      :found-non-whitespace
      (do
        (r/unread in c)
        context))))

(defn ^:private unexpected-eof
  [parsing context]
  {:pre [(keyword? parsing)
         (string? context)]}
  {:status :error
   :error-type :unexpected-end-of-input
   :parsing parsing
   :context context})

(defn ^:private unexpected-token
  [parsing context expected actual]
  {:pre [(keyword? parsing)
         (string? context)]}
  (if (some? actual)
    {:status :error
     :error-type :unexpected-token
     :parsing parsing
     :context context
     :expected expected
     :actual actual}
    (unexpected-eof parsing context)))

(defn ^:private parse-ok
  [parsing context]
  {:pre [(keyword? parsing)
         (string? context)]}
  {:status :ok
   :parsing parsing
   :context context
   :value (.parse js/JSON context)})

(defn ^:private read-literal
  [in parsing]
  (loop [literal-chars (name parsing)
         context ""]
    (if (empty? literal-chars)
      (parse-ok parsing context)
      (let [expected (first literal-chars)
            actual (r/read-char in)]
        (if (= expected actual)
          (recur (next literal-chars)
                 (str context actual))
          (unexpected-token parsing context expected actual))))))

(defn read-null
  [in]
  (read-literal in :null))

(defn read-true
  [in]
  (read-literal in :true))

(defn read-false
  [in]
  (read-literal in :false))

(def ^:private hyphen? #{\-})
(def ^:private digit? #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
(def ^:private digit1-9? #{\1 \2 \3 \4 \5 \6 \7 \8 \9})
(def ^:private digit0? #{\0})
(def ^:private fractional-marker? #{\.})
(def ^:private exponent-marker? #{\e \E})
(def ^:private exponent-sign? #{\+ \-})

(defn read-number
  "Reads a JSON number from the push-back reader.  Returns a map with the
  result."
  [in]
  (letfn [(look-for-hyphen []
            (let [c (r/read-char in)]
              (cond
                (hyphen? c)
                #(first-int-digit "-")

                (digit? c)
                (do
                  (r/unread in c)
                  #(first-int-digit ""))

                :else
                (unexpected-token :number "" (set/union hyphen? digit?) c))))

          (first-int-digit [context]
            (let [c (r/read-char in)]
              (cond
                (digit0? c)
                #(int-part-done (str context c))

                (digit1-9? c)
                #(more-int-digits (str context c))

                :else
                (unexpected-token :number context (set/union digit0? digit1-9?) c))))

          (more-int-digits [context]
            (let [c (r/read-char in)]
              (if (digit? c)
                #(more-int-digits (str context c))
                (do
                  (r/unread in c)
                  #(int-part-done context)))))

          (int-part-done [context]
            (let [c (r/read-char in)]
              (cond
                (fractional-marker? c)
                #(first-frac-digit (str context c))

                (exponent-marker? c)
                #(exponent-sign (str context c))

                :else
                (do
                  (r/unread in c)
                  (parse-ok :number context)))))

          (first-frac-digit [context]
            (let [c (r/read-char in)]
              (cond
                (digit? c)
                #(more-frac-digits (str context c))

                :else
                (unexpected-token :number context digit? c))))

          (more-frac-digits [context]
            (let [c (r/read-char in)]
              (cond
                (digit? c)
                #(more-frac-digits (str context c))

                (exponent-marker? c)
                #(exponent-sign (str context c))

                :else
                (do
                  (r/unread in c)
                  (parse-ok :number context)))))

          (exponent-sign [context]
            (let [c (r/read-char in)]
              (cond
                (exponent-sign? c)
                #(exponent-magnitude (str context c))

                (digit? c)
                #(more-exponent-magnitude (str context c))

                :else
                (unexpected-token :number context (set/union exponent-sign? digit?) c))))

          (exponent-magnitude [context]
            (let [c (r/read-char in)]
              (cond
                (digit? c)
                #(more-exponent-magnitude (str context c))

                :else
                (unexpected-token :number context (set/union digit?) c))))

          (more-exponent-magnitude [context]
            (let [c (r/read-char in)]
              (cond
                (digit? c)
                #(more-exponent-magnitude (str context c))

                :else
                (do
                  (r/unread in c)
                  (parse-ok :number context)))))]
    (trampoline look-for-hyphen)))

(def ^:private quote? #{\"})
(def ^:private control-character? (into #{}
                                        (map char)
                                        (range 0x0000 0x0020)))
(def ^:private reverse-solidus? #{\\})
(def ^:private unicode-escape-char? #{\u})
(def ^:private escape-char? #{\" \\ \/ \b \f \n \r \t})
(def ^:private hex-digit?
  (set/union digit?
             #{\a \b \c \d \e \f}
             #{\A \B \C \D \E \F}))

(defn read-string
  "Reads a string from the push-back reader."
  [in]
  (letfn [(look-for-quote []
            (let [c (r/read-char in)]
              (if (quote? c)
                #(next-char \")
                (unexpected-token :string "" \" c))))

          (next-char [context]
            (let [c (r/read-char in)]
              (cond
                (quote? c)
                (parse-ok :string (str context c))

                (control-character? c)
                (unexpected-token :string context :string-content c)

                (reverse-solidus? c)
                #(escaped-char (str context c))

                (nil? c)
                (unexpected-token :string context :string-content c)

                :otherwise
                #(next-char (str context c)))))

          (escaped-char [context]
            (let [c (r/read-char in)]
              (cond
                (escape-char? c)
                #(next-char (str context c))

                (unicode-escape-char? c)
                #(unicode-escape (str context c) 4)

                :invalid-escape-char
                (unexpected-token :string context (set/union escape-char? unicode-escape-char?) c))))

          (unicode-escape [context digits-left]
            (if (zero? digits-left)
              #(next-char context)
              (let [c (r/read-char in)]
                (if (hex-digit? c)
                  #(unicode-escape (str context c) (dec digits-left))
                  (unexpected-token :string context hex-digit? c)))))]
    (trampoline look-for-quote)))

(declare read-value)

(defn read-array
  "Reads a JSON array from the push-back reader."
  [in]
  (letfn [(open-array []
            (let [c (r/read-char in)]
              (if (= \[ c)
                first-value
                (unexpected-token :array "" \[ c))))

          (first-value []
            (let [context (str "[" (consume-whitespace! in))
                  c (r/read-char in)]
              (cond
                (= \] c)
                (emit-array (str context c) [])

                :maybe-a-value
                (do
                  (when (some? c)
                    (r/unread in c))
                  #(next-value context [])))))

          (comma-or-end [context values]
            (let [context (str context (consume-whitespace! in))
                  c (r/read-char in)]
              (cond
                (= \] c)
                (emit-array (str context c) values)

                (= \, c)
                #(next-value (str context c) values)

                :otherwise
                (unexpected-token :array context #{\] \,} c))))

          (next-value [context values]
            (let [context (str context (consume-whitespace! in))
                  c (r/read-char in)]
              (if (nil? c)
                (unexpected-eof :array context)
                (do
                  (r/unread in c)
                  (let [{:keys [status value] inner-context :context :as cause} (read-value in)]
                    (if (= :ok status)
                      #(comma-or-end (str context inner-context) (conj values value))
                      {:status :error
                       :error-type :bad-array-value
                       :parsing :array
                       :context context
                       :cause cause}))))))

          (emit-array [context values]
            {:status :ok
             :parsing :array
             :context context
             :value (apply array values)})]
    (trampoline open-array)))

(defn read-object
  "Reads a JSON object from the push-back reader."
  [in]
  (letfn [(open-object []
            (let [c (r/read-char in)]
              (if (= \{ c)
                first-entry
                (unexpected-token :object "" \{ c))))

          (first-entry []
            (let [context (str "{" (consume-whitespace! in))
                  c (r/read-char in)]
              (cond
                (= \} c)
                (emit-object (str context c) [])

                :maybe-a-value
                (do
                  (when (some? c)
                    (r/unread in c))
                  #(next-key context [])))))

          (next-key [context keyvals]
            (let [context (str context (consume-whitespace! in))
                  c (r/read-char in)]
              (if (nil? c)
                (unexpected-eof :object context)
                (do
                  (r/unread in c)
                  (let [{:keys [status value] inner-context :context :as cause} (read-string in)]
                    (if (= :ok status)
                      #(colon (str context inner-context) (conj keyvals value))
                      {:status :error
                       :error-type :bad-object-key
                       :parsing :object
                       :context context
                       :cause cause}))))))

          (colon [context keyvals]
            (let [context (str context (consume-whitespace! in))
                  c (r/read-char in)]
              (cond
                (= \: c)
                #(next-value (str context c) keyvals)

                :otherwise
                (unexpected-token :object context \: c))))

          (next-value [context values]
            (let [context (str context (consume-whitespace! in))
                  c (r/read-char in)]
              (if (nil? c)
                (unexpected-eof :object context)
                (do
                  (r/unread in c)
                  (let [{:keys [status value] inner-context :context :as cause} (read-value in)]
                    (if (= :ok status)
                      #(comma-or-end (str context inner-context) (conj values value))
                      {:status :error
                       :error-type :bad-object-value
                       :parsing :object
                       :context context
                       :cause cause}))))))

          (comma-or-end [context keyvals]
            (let [context (str context (consume-whitespace! in))
                  c (r/read-char in)]
              (cond
                (= \} c)
                (emit-object (str context c) keyvals)

                (= \, c)
                #(next-key (str context c) keyvals)

                :otherwise
                (unexpected-token :object context #{\} \,} c))))

          (emit-object [context keyvals]
            {:status :ok
             :parsing :object
             :context context
             :value (apply js-obj keyvals)})]
    (trampoline open-object)))

(defn read-value
  "Reads an arbitrary JSON value from the push-back reader.  Does not consume
  initial white space."
  [in]
  (let [c (r/read-char in)]
    (cond
      (= \[ c)
      (do
        (r/unread in c)
        (read-array in))

      (= \{ c)
      (do
        (r/unread in c)
        (read-object in))

      (or (hyphen? c)
          (digit? c))
      (do
        (r/unread in c)
        (read-number in))

      (quote? c)
      (do
        (r/unread in c)
        (read-string in))

      (= \n c)
      (do
        (r/unread in c)
        (read-null in))

      (= \t c)
      (do
        (r/unread in c)
        (read-true in))

      (= \f c)
      (do
        (r/unread in c)
        (read-false in))

      (nil? c)
      (unexpected-eof :value "")

      :all-others
      (unexpected-token :value "" #{:object :array :number :string :true :false :null} c))))

(defn read-json
  "Reads a top-level JSON value (object or array) from the push back reader.
  Consumes initial white space."
  [in]
  (consume-whitespace! in)
  (let [c (r/read-char in)]
    (cond
      (= \[ c)
      (do
        (r/unread in c)
        (read-array in))

      (= \{ c)
      (do
        (r/unread in c)
        (read-object in))

      (nil? c)
      (unexpected-eof :json "")

      :all-others
      (unexpected-token :json "" #{:object :array} c))))

(defn read
  "A function that behaves much like cljs.reader/read, except that it reads
  JSON instead of EDN."
  [rdr eof-is-error sentinel is-recursive]
  (let [{:keys [status value] :as result} (read-json rdr)]
    (cond
      (= :ok status)
      value

      (and (= :error status)
           (= :json (:parsing result))
           (= "" (:context result))
           (= :unexpected-end-of-input (:error-type result))
           (not eof-is-error))
      sentinel

      :otherwise
      (throw (ex-info "Failed to parse JSON" result)))))
