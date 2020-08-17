(ns dang.main
  (:require [clojure.string]
            [clojure.repl]
            [dang.parser]))

(defn println-error [& more]
  (.println *err* (clojure.string/join " " more)))

(defn -main []
  (let [syntax-str (read-line)
        ast (dang.parser/parse-ast syntax-str)]
    (println-error ast)
    (if (:reason ast) ;; :reason means errors
      (System/exit 2)
      (System/exit 3)))) ;; assume no typecheck

