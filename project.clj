(defproject watershed "0.1.0"
  :description "Library for working with probabilistic graphical models in Clojure."
  :url "https://github.com/klgraham/watershed"

  :lein-release {:deploy-via :clojars}

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [prismatic/schema "0.1.9"]
                 [org.clojure/core.match "0.2.0"]
                 [com.taoensso/timbre "3.0.0-RC4"]]

  :profiles {:dev {:dependencies [[midje "1.5.1"]]}
             :test-libs {:dependencies [[midje "1.6.3"]]}
             :1.4       [:test-libs {:dependencies [[org.clojure/clojure "1.4.0"]]}]
             :1.5.0     [:test-libs {:dependencies [[org.clojure/clojure "1.5.0"]]}]
             :1.5.1     [:test-libs {:dependencies [[org.clojure/clojure "1.5.1"]]}]
             :1.6       [:test-libs {:dependencies [[org.clojure/clojure "1.6.0-beta2"]]}]}

  :aliases {"compatibility" ["with-profile" "1.4:1.5.0:1.5.1:1.6" "midje" ]})
