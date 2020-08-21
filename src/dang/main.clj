(ns dang.main
  (:require [clojure.string]
            [clojure.repl]
            [dang.parser]
            [dang.typechecker]
            [dang.evaluate]))

(defn eval-pcf [str]
  (let [syntax-str (clojure.string/trim str)
        ast (dang.parser/parse-ast syntax-str)]
    (if (:reason ast)
      {:parse-error  (:reason ast)}
      (if (nil? (dang.typechecker/typecheck ast))
        :type-error
        ;; TODO: error handling for botched evaluation
        (dang.evaluate/evaluate ast)))))

;; TODO: figure out how to update socket server without repl restart
(defn server []
  (let [input (read-line)]
    (println (eval-pcf input))))

(comment
  (nil? (dang.typechecker/typecheck (dang.parser/parse-ast "true")))
  (nil? (dang.typechecker/typecheck (dang.parser/parse-ast "is-zero 1")))
  (nil? (dang.typechecker/typecheck (dang.parser/parse-ast "0 1")))
  (keyword "dang.parser" "nat"))