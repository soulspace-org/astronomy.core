(defproject org.soulspace.clj/astronomy.core "0.4.4-SNAPSHOT"
  :description "A library of astronomical algorithms in clojure"
  :url "https://github.com/soulspace-org/astronomy.core"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  ; use deps.edn dependencies
;  :plugins [[lein-tools-deps "0.4.5"]]
;  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
;  :lein-tools-deps/config {:config-files [:install :user :project]}

  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/spec.alpha "0.3.218"]
                 [org.soulspace.clj/clj.base "0.9.1"]
                 [org.soulspace.clj/math.core "0.9.0"]]

  :test-paths ["test"]

  :profiles {:dev {:dependencies [[djblue/portal "0.37.1"]
                                  [criterium "0.4.6"]]
                   :global-vars {*warn-on-reflection* true}}}

  :scm {:name "git" :url "https://github.com/soulspace-org/astronomy.core"}
  :deploy-repositories [["clojars"  {:sign-releases false :url "https://clojars.org/repo"}]])
