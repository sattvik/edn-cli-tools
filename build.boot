(set-env! :dependencies '[[cljsjs/nodejs-externs "1.0.4-1"]
                          [com.cognitect/transit-cljs "0.8.239"]
                          [org.clojure/clojurescript "1.9.562"]
                          [org.clojure/tools.cli "0.3.5"]

                          [adzerk/boot-cljs "2.0.0" :scope "test"]
                          [adzerk/boot-cljs-repl "0.3.2" :scope "test"]
                          [com.cemerick/piggieback "0.2.2" :scope "test"]
                          [crisptrutski/boot-cljs-test "0.3.1" :scope "test"]
                          [org.clojure/clojure "1.8.0" :scope "test"]
                          [org.clojure/tools.nrepl "0.2.13" :scope "test"]
                          [weasel "0.7.0" :scope "test"]]
          :source-paths #{"src"}
          :exclusions '[org.clojure/clojure
                        org.clojure/clojurescript])

(require '[adzerk.boot-cljs :refer [cljs]]
         '[adzerk.boot-cljs-repl :refer [cljs-repl]]
         '[cljs.build.api :as cljs-build]
         '[clojure.java.io :as io]
         '[crisptrutski.boot-cljs-test :refer [test-cljs]])

(task-options! cljs {:compiler-options {:target :nodejs
                                        :language-in :ecmascript5-strict
                                        :infer-externs true}}
               test-cljs {:js-env :node})

(deftask with-tests
  []
  (merge-env! :source-paths #{"test"})
  identity)

(deftask dev
  []
  (comp (with-tests)
        (watch)
        (cljs-repl)
        (test-cljs)
        (cljs :source-map true
              :optimizations :none)
        (target)))

(deftask prod
  []
  (comp (cljs :optimizations :advanced)
        (sift :include #{#"main\.js"})
        (target)))
