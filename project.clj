(defproject
  solutions-4clojure
  "0.1.0-SNAPSHOT"
  :dependencies
  [[org.clojure/clojure "1.9.0-alpha15"]
   [adzerk/boot-test "1.2.0" :scope "test"]
   [boot/core "2.6.0" :scope "compile"]]
  :repositories
  [["clojars" {:url "https://repo.clojars.org/"}]
   ["maven-central" {:url "https://repo1.maven.org/maven2"}]]
  :source-paths
  ["test" "src" "resources"])