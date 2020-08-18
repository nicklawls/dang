(ns dang.main
  (:require [clojure.string]
            [clojure.repl]
            [dang.parser]
            [dang.typechecker]))

(defn println-error [& more]
  (.println *err* (clojure.string/join " " more)))

(defn -main []
  (let [syntax-str (read-line)
        ast (dang.parser/parse-ast syntax-str)]
    (println-error ast)
    (if (:reason ast) ;; :reason means errors
      (System/exit 2)
      (if
       (nil? (dang.typechecker/typecheck ast))
        (System/exit 3)
        (println "not done yet son")))))

(comment
  (nil? (dang.typechecker/typecheck (dang.parser/parse-ast "true")))
  (nil? (dang.typechecker/typecheck (dang.parser/parse-ast "is-zero 1")))
  (nil? (dang.typechecker/typecheck (dang.parser/parse-ast "0 1")))
  (keyword "dang.parser" "nat"))