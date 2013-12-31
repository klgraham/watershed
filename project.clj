(defproject probable-clj "0.1.0-SNAPSHOT"
  :description "Library for working with probabilistic graphical models in Clojure."
  :url "https://github.com/klgraham/probable-clj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [prismatic/schema "0.1.9"]
                 [org.clojure/core.match "0.2.0"]]
  :profiles {:dev {:dependencies [[midje "1.5.1"]]}})
