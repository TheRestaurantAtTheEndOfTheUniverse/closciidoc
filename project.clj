(defproject closciidoc "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :plugins [[lein-cloverage "1.0.2"]]
  :profiles {:dev {:dependencies [[midje "1.6.3"]]}}
  :main closciidoc.core)
