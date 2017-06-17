(ns io.clojure.edn-cli-tools.reader.json-test
  (:require [clojure.test :refer [deftest is]]
            [cljs.reader :as r]
            [io.clojure.edn-cli-tools.reader.json :as jr]))

(deftest test-read-null
  (is (= {:status :error
          :parsing :null
          :error-type :unexpected-end-of-input
          :context ""}
         (jr/read-null (r/push-back-reader "")))
      "reading empty input")
  (is (= {:status :error
          :parsing :null
          :error-type :unexpected-end-of-input
          :context "n"}
         (jr/read-null (r/push-back-reader "n")))
      "unexpected end of input")
  (is (= {:status :error
          :parsing :null
          :error-type :unexpected-token
          :context "n"
          :expected \u
          :actual \n}
         (jr/read-null (r/push-back-reader "nn")))
      "unexpected character")
  (is (= {:status :ok
          :parsing :null
          :context "null"
          :value nil}
         (jr/read-null (r/push-back-reader "null")))
      "reads fine")
  (is (= {:status :ok
          :parsing :null
          :context "null"
          :value nil}
         (jr/read-null (r/push-back-reader "nulln")))
      "ignores extra input"))

(deftest test-read-true
  (is (= {:status :error
          :parsing :true
          :error-type :unexpected-end-of-input
          :context ""}
         (jr/read-true (r/push-back-reader "")))
      "reading empty input")
  (is (= {:status :error
          :parsing :true
          :error-type :unexpected-end-of-input
          :context "t"}
         (jr/read-true (r/push-back-reader "t")))
      "unexpected end of input")
  (is (= {:status :error
          :parsing :true
          :error-type :unexpected-token
          :context "t"
          :expected \r
          :actual \n}
         (jr/read-true (r/push-back-reader "tn")))
      "unexpected character")
  (is (= {:status :ok
          :parsing :true
          :context "true"
          :value true}
         (jr/read-true (r/push-back-reader "true")))
      "reads fine")
  (is (= {:status :ok
          :parsing :true
          :context "true"
          :value true}
         (jr/read-true (r/push-back-reader "truen")))
      "ignores extra input"))

(deftest test-read-false
  (is (= {:status :error
          :parsing :false
          :error-type :unexpected-end-of-input
          :context ""}
         (jr/read-false (r/push-back-reader "")))
      "reading empty input")
  (is (= {:status :error
          :parsing :false
          :error-type :unexpected-end-of-input
          :context "f"}
         (jr/read-false (r/push-back-reader "f")))
      "unexpected end of input")
  (is (= {:status :error
          :parsing :false
          :error-type :unexpected-token
          :context "f"
          :expected \a
          :actual \n}
         (jr/read-false (r/push-back-reader "fn")))
      "unexpected character")
  (is (= {:status :ok
          :parsing :false
          :context "false"
          :value false}
         (jr/read-false (r/push-back-reader "false")))
      "reads fine")
  (is (= {:status :ok
          :parsing :false
          :context "false"
          :value false}
         (jr/read-false (r/push-back-reader "falsen")))
      "ignores extra input"))

(deftest test-read-number
  (is (= {:status :error
          :parsing :number
          :error-type :unexpected-end-of-input
          :context ""}
         (jr/read-number (r/push-back-reader "")))
      "reading empty input")
  (is (= {:status :error
          :parsing :number
          :error-type :unexpected-end-of-input
          :context "-"}
         (jr/read-number (r/push-back-reader "-")))
      "only hyphen")
  (is (= {:status :error
          :parsing :number
          :error-type :unexpected-token
          :context "-"
          :expected #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9}
          :actual \-}
         (jr/read-number (r/push-back-reader "--")))
      "two hyphens")
  (is (= {:status :ok
          :parsing :number
          :context "0"
          :value 0}
         (jr/read-number (r/push-back-reader "0")))
      "reading 0")
  (is (= {:status :ok
          :parsing :number
          :context "-0"
          :value 0}
         (jr/read-number (r/push-back-reader "-0")))
      "reading -0")
  (is (= {:status :ok
          :parsing :number
          :context "12"
          :value 12}
         (jr/read-number (r/push-back-reader "12")))
      "reading 12")
  (is (= {:status :ok
          :parsing :number
          :context "12"
          :value 12}
         (jr/read-number (r/push-back-reader "12t")))
      "ignoring extra stuff after integer part")
  (is (= {:status :error
          :parsing :number
          :error-type :unexpected-end-of-input
          :context "12."}
         (jr/read-number (r/push-back-reader "12.")))
      "dot with no fractional digits")
  (is (= {:status :error
          :parsing :number
          :error-type :unexpected-token
          :context "12."
          :expected #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9}
          :actual \f}
         (jr/read-number (r/push-back-reader "12.f")))
      "dot with no fractional digits")
  (is (= {:status :error
          :parsing :number
          :error-type :unexpected-token
          :context "12."
          :expected #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9}
          :actual \e}
         (jr/read-number (r/push-back-reader "12.e10")))
      "dot with no fractional digits")
  (is (= {:status :ok
          :parsing :number
          :context "3.14"
          :value 3.14}
         (jr/read-number (r/push-back-reader "3.14x")))
      "reading 3.14x")
  (is (= {:status :ok
          :parsing :number
          :context "3.14e0"
          :value 3.14}
         (jr/read-number (r/push-back-reader "3.14e0")))
      "reading 3.14e0")
  (is (= {:status :ok
          :parsing :number
          :context "3.14E0"
          :value 3.14}
         (jr/read-number (r/push-back-reader "3.14E0")))
      "reading 3.14E0")
  (is (= {:status :ok
          :parsing :number
          :context "3.14e-0"
          :value 3.14}
         (jr/read-number (r/push-back-reader "3.14e-0")))
      "reading 3.14e-0")
  (is (= {:status :ok
          :parsing :number
          :context "3.14E-0"
          :value 3.14}
         (jr/read-number (r/push-back-reader "3.14E-0")))
      "reading 3.14E-0")
  (is (= {:status :ok
          :parsing :number
          :context "3.14e+0"
          :value 3.14}
         (jr/read-number (r/push-back-reader "3.14e+0")))
      "reading 3.14e+0")
  (is (= {:status :ok
          :parsing :number
          :context "3.14E+0"
          :value 3.14}
         (jr/read-number (r/push-back-reader "3.14E+0")))
      "reading 3.14E+0")
  (is (= {:status :error
          :parsing :number
          :error-type :unexpected-end-of-input
          :context "12e"}
         (jr/read-number (r/push-back-reader "12e")))
      "exponent without magnitude")
  (is (= {:status :error
          :parsing :number
          :error-type :unexpected-end-of-input
          :context "12E+"}
         (jr/read-number (r/push-back-reader "12E+")))
      "exponent with sign but no magnitude"))

(deftest read-string-test
  (is (= {:status :error
          :error-type :unexpected-end-of-input
          :parsing :string
          :context ""}
         (jr/read-string (r/push-back-reader "")))
      "empty input")
  (is (= {:status :error
          :error-type :unexpected-token
          :parsing :string
          :context ""
          :expected \"
          :actual \a}
         (jr/read-string (r/push-back-reader "a")))
      "non-quote")
  (is (= {:status :ok
          :parsing :string
          :context "\"\""
          :value ""}
         (jr/read-string (r/push-back-reader "\"\"")))
      "empty string")
  (is (= {:status :error
          :error-type :unexpected-end-of-input
          :parsing :string
          :context "\""}
         (jr/read-string (r/push-back-reader "\"")))
      "open string")
  (is (= {:status :ok
          :parsing :string
          :context "\"\\\"\""
          :value "\""}
         (jr/read-string (r/push-back-reader "\"\\\"\"")))
      "quote")
  (is (= {:status :ok
          :parsing :string
          :context "\"\\\\\""
          :value "\\"}
         (jr/read-string (r/push-back-reader "\"\\\\\"")))
      "reverse solidus")
  (is (= {:status :ok
          :parsing :string
          :context "\"\\/\""
          :value "/"}
         (jr/read-string (r/push-back-reader "\"\\/\"")))
      "solidus")
  (is (= {:status :ok
          :parsing :string
          :context "\"\\b\""
          :value "\u0008"}
         (jr/read-string (r/push-back-reader "\"\\b\"")))
      "backspace")
  (is (= {:status :ok
          :parsing :string
          :context "\"\\f\""
          :value "\u000c"}
         (jr/read-string (r/push-back-reader "\"\\f\"")))
      "form feed")
  (is (= {:status :ok
          :parsing :string
          :context "\"\\n\""
          :value "\u000a"}
         (jr/read-string (r/push-back-reader "\"\\n\"")))
      "line feed")
  (is (= {:status :ok
          :parsing :string
          :context "\"\\r\""
          :value "\u000d"}
         (jr/read-string (r/push-back-reader "\"\\r\"")))
      "carriage return")
  (is (= {:status :ok
          :parsing :string
          :context "\"\\t\""
          :value "\u0009"}
         (jr/read-string (r/push-back-reader "\"\\t\"")))
      "tabulation")
  (is (= {:status :ok
          :parsing :string
          :context "\"\\u0000\""
          :value "\u0000"}
         (jr/read-string (r/push-back-reader "\"\\u0000\"")))
      "unicode escape")
  (is (= {:status :error
          :error-type :unexpected-token
          :parsing :string
          :context "\"\\"
          :expected #{\" \\ \/ \b \f \n \r \t \u}
          :actual \g}
         (jr/read-string (r/push-back-reader "\"\\g\"")))
      "invalid escape")
  (is (= {:status :error
          :error-type :unexpected-token
          :parsing :string
          :context "\""
          :expected :string-content
          :actual \newline}
         (jr/read-string (r/push-back-reader "\"\n\"")))
      "control character")
  (is (= {:status :error
          :error-type :unexpected-token
          :parsing :string
          :context "\"\\u"
          :expected #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \a \b \c \d \e \f \A \B \C \D \E \F}
          :actual \"}
         (jr/read-string (r/push-back-reader "\"\\u\"")))
      "incomplete unicode escape 0")
  (is (= {:status :error
          :error-type :unexpected-token
          :parsing :string
          :context "\"\\u0"
          :expected #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \a \b \c \d \e \f \A \B \C \D \E \F}
          :actual \"}
         (jr/read-string (r/push-back-reader "\"\\u0\"")))
      "incomplete unicode escape 1")
  (is (= {:status :error
          :error-type :unexpected-token
          :parsing :string
          :context "\"\\u00"
          :expected #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \a \b \c \d \e \f \A \B \C \D \E \F}
          :actual \x}
         (jr/read-string (r/push-back-reader "\"\\u00x\"")))
      "incomplete unicode escape 2")
  (is (= {:status :error
          :error-type :unexpected-token
          :parsing :string
          :context "\"\\u000"
          :expected #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \a \b \c \d \e \f \A \B \C \D \E \F}
          :actual \x}
         (jr/read-string (r/push-back-reader "\"\\u000x\"")))
      "incomplete unicode escape 3"))

(deftest test-read-array
  (is (= {:status :error
          :error-type :unexpected-end-of-input
          :parsing :array
          :context ""}
         (jr/read-array (r/push-back-reader "")))
      "empty input")
  (is (= {:status :error
          :error-type :unexpected-token
          :parsing :array
          :context ""
          :expected \[
          :actual \b}
         (jr/read-array (r/push-back-reader "blah")))
      "not an array")
  (is (= (js->clj {:status :ok
                   :parsing :array
                   :context "[]"
                   :value #js []})
         (js->clj (jr/read-array (r/push-back-reader "[]"))))
      "empty array")
  (is (= (js->clj {:status :ok
                   :parsing :array
                   :context "[   ]"
                   :value #js []})
         (js->clj (jr/read-array (r/push-back-reader "[   ]"))))
      "empty array, with whitespace")
  (is (= {:status :error
          :error-type :unexpected-end-of-input
          :parsing :array
          :context "[ "}
         (jr/read-array (r/push-back-reader "[ ")))
      "open array, no values")
  (is (= {:status :error
          :error-type :unexpected-end-of-input
          :parsing :array
          :context "[ 1"}
         (jr/read-array (r/push-back-reader "[ 1")))
      "open array, one value, missing comma")
  (is (= {:status :error
          :error-type :unexpected-end-of-input
          :parsing :array
          :context "[ 1 , "}
         (jr/read-array (r/push-back-reader "[ 1 , ")))
      "open array, one value, with comma")
  (is (= (js->clj {:status :ok
                   :parsing :array
                   :context "[ 1 ]"
                   :value #js [1]})
         (js->clj (jr/read-array (r/push-back-reader "[ 1 ]"))))
      "array with one value")
  (is (= (js->clj {:status :ok
                   :parsing :array
                   :context "[ 1,\n2]"
                   :value #js [1 2]})
         (js->clj (jr/read-array (r/push-back-reader "[ 1,\n2]"))))
      "array with two values")
  (is (= {:status :error
          :error-type :bad-array-value
          :parsing :array
          :context "[ "
          :cause {:status :error
                  :error-type :unexpected-token
                  :parsing :value
                  :context ""
                  :expected #{:object :array :number :string :true :false :null}
                  :actual \x}}
         (jr/read-array (r/push-back-reader "[ x ]")))
      "array with invalid value")
  (is (= {:status :error
          :error-type :bad-array-value
          :parsing :array
          :context "[ 1, "
          :cause {:status :error
                  :error-type :unexpected-end-of-input
                  :parsing :string
                  :context "\"foo ]"}}
         (jr/read-array (r/push-back-reader "[ 1, \"foo ]")))
      "array with an unterminated string")
  (is (= {:status :error
          :error-type :bad-array-value
          :parsing :array
          :context "[ 1, "
          :cause {:status :error
                  :error-type :unexpected-token
                  :parsing :number
                  :context "0."
                  :expected #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9}
                  :actual \space}}
         (jr/read-array (r/push-back-reader "[ 1, 0. ]")))
      "array with an invalid number")
  (is (= {:status :error
          :error-type :bad-array-value
          :parsing :array
          :context "[[],"
          :cause {:status :error
                  :error-type :bad-array-value
                  :parsing :array
                  :context "["
                  :cause {:status :error
                          :error-type :unexpected-token
                          :parsing :value
                          :context ""
                          :expected #{:object :array :number :string :true :false :null}
                          :actual \,}}}
         (jr/read-array (r/push-back-reader "[[],[,]")))
      "unclosed nested arrays"))

(deftest test-read-object
  (is (= {:status :error
          :error-type :unexpected-end-of-input
          :parsing :object
          :context ""}
         (jr/read-object (r/push-back-reader "")))
      "empty input")
  (is (= {:status :error
          :error-type :unexpected-end-of-input
          :parsing :object
          :context "{  "}
         (jr/read-object (r/push-back-reader "{  ")))
      "unclosed object")
  (is (= (js->clj {:status :ok
                   :parsing :object
                   :context "{  }"
                   :value #js {}})
         (js->clj (jr/read-object (r/push-back-reader "{  }"))))
      "empty object")
  (is (= {:status :error
          :error-type :bad-object-key
          :parsing :object
          :context "{"
          :cause {:status :error
                  :error-type :unexpected-token
                  :parsing :string
                  :context ""
                  :expected \"
                  :actual \1}}
         (jr/read-object (r/push-back-reader "{1:1}")))
      "initial bad key")
  (is (= {:status :error
          :error-type :unexpected-end-of-input
          :parsing :object
          :context "{\"key\" "}
         (jr/read-object (r/push-back-reader "{\"key\" ")))
      "missing colon, end of input")
  (is (= {:status :error
          :error-type :unexpected-token
          :parsing :object
          :context "{\"key\" "
          :expected \:
          :actual \1}
         (jr/read-object (r/push-back-reader "{\"key\" 1 }")))
      "missing colon")
  (is (= {:status :error
          :error-type :unexpected-end-of-input
          :parsing :object
          :context "{\"key\":"}
         (jr/read-object (r/push-back-reader "{\"key\":")))
      "missing value (end of input")
  (is (= {:status :error
          :error-type :bad-object-value
          :parsing :object
          :context "{\"key\":"
          :cause {:status :error
                  :error-type :unexpected-token
                  :parsing :value
                  :context ""
                  :expected #{:object :array :number :string :true :false :null}
                  :actual \}}}
         (jr/read-object (r/push-back-reader "{\"key\":}")))
      "missing value")
  (is (= {:status :error
          :error-type :unexpected-end-of-input
          :parsing :object
          :context "{\"key\":1"}
         (jr/read-object (r/push-back-reader "{\"key\":1")))
      "no termination after first entry")
  (is (= {:status :error
          :error-type :unexpected-end-of-input
          :parsing :object
          :context "{\"key\": 1, "}
         (jr/read-object (r/push-back-reader "{\"key\": 1, ")))
      "no second entry after comma")
  (is (= (js->clj {:status :ok
                   :parsing :object
                   :context "{\"a\": 1}"
                   :value #js {"a" 1}})
         (js->clj (jr/read-object (r/push-back-reader "{\"a\": 1}"))))
      "one entry")
  (is (= (js->clj {:status :ok
                   :parsing :object
                   :context "{\"a\": 1, \"b\": 2}"
                   :value #js {"a" 1 "b" 2}})
         (js->clj (jr/read-object (r/push-back-reader "{\"a\": 1, \"b\": 2}"))))
      "two entries")
  (is (= (js->clj {:status :ok
                   :parsing :object
                   :context "{\"a\": {\"b\": 2}}"
                   :value #js {"a" #js {"b" 2}}})
         (js->clj (jr/read-object (r/push-back-reader "{\"a\": {\"b\": 2}}"))))
      "nested map"))

(deftest test-read-value
  (is (= (js->clj {:status :ok
                   :parsing :object
                   :context "{\"answer\": 42}"
                   :value #js {"answer" 42}})
         (js->clj (jr/read-value (r/push-back-reader "{\"answer\": 42}"))))
      "reading an object")
  (is (= (js->clj {:status :ok
                   :parsing :array
                   :context "[1]"
                   :value #js [1]})
         (js->clj (jr/read-value (r/push-back-reader "[1]"))))
      "reading an array")
  (is (= {:status :ok
          :parsing :string
          :context "\"foo\""
          :value "foo"}
         (jr/read-value (r/push-back-reader "\"foo\"")))
      "reading a string")
  (is (= {:status :ok
          :parsing :true
          :context "true"
          :value true}
         (jr/read-value (r/push-back-reader "true")))
      "reading a true")
  (is (= {:status :ok
          :parsing :false
          :context "false"
          :value false}
         (jr/read-value (r/push-back-reader "false")))
      "reading a false")
  (is (= {:status :ok
          :parsing :null
          :context "null"
          :value nil}
         (jr/read-value (r/push-back-reader "null")))
      "reading a null")

  (is (= {:status :error
          :error-type :unexpected-end-of-input
          :parsing :value
          :context ""}
         (jr/read-value (r/push-back-reader "")))
      "unexpected eof")
  (is (= {:status :error
          :error-type :unexpected-token
          :parsing :value
          :context ""
          :expected #{:object :array :number :string :true :false :null}
          :actual \q}
         (jr/read-value (r/push-back-reader "q")))
      "unexpected token")
  (is (= {:status :error
          :error-type :unexpected-token
          :parsing :value
          :expected #{:object :array :number :string :true :false :null}
          :context ""
          :actual \newline}
         (jr/read-value (r/push-back-reader "\n\n \"foo\"")))
      "leading whitespace"))


(deftest test-read
  (is (= ::eof
         (jr/read (r/push-back-reader "") false ::eof false))
      "eof with sentinel")
  (is (= ::eof
         (jr/read (r/push-back-reader "\n\n") false ::eof false))
      "white space with sentinel")
  (is (thrown? js/Error (jr/read (r/push-back-reader "") true nil false))
      "eof without sentinel")
  (is (thrown? js/Error (jr/read (r/push-back-reader "\n\n") true nil false))
      "white space without sentinel")
  (is (= (js->clj #js [1 2])
         (js->clj (jr/read (r/push-back-reader "[1,2] [") false ::eof false)))
      "read array")
  (is (= (js->clj #js {"a" 1})
         (js->clj (jr/read (r/push-back-reader "{\"a\": 1} [") false ::eof false)))
      "read object")
  (is (thrown? js/Error (jr/read (r/push-back-reader "[1,2") false ::eof false))
      "eof inside of array"))
