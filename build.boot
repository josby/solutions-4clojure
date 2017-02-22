(def project 'solutions-4clojure)
(def version "0.1.0-SNAPSHOT")

(set-env! :resource-paths #{"resources" "src"}
          :source-paths   #{"test"}
          :dependencies   '[[org.clojure/clojure "RELEASE"]
                            [adzerk/boot-test "RELEASE" :scope "test"]])

(task-options!
 aot {:namespace   #{'solutions-4clojure.core}}
 pom {:project     project
      :version     version
      :description "My solutions to 4clojure problems"
      :scm         {:url "https://github.com/josby/solutions-4clojure"}
      :license     {"Eclipse Public License"
                    "http://www.eclipse.org/legal/epl-v10.html"}})


(require '[adzerk.boot-test :refer [test]])
