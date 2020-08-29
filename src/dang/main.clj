(ns dang.main
  (:require [clojure.string]
            [clojure.repl]
            [dang.parser]
            [dang.typechecker]
            [dang.evaluate]))

(defn eval-pcf [str]
  (try
    (let [syntax-str (clojure.string/trim str)
          ast (dang.parser/parse-ast syntax-str)]
      (if (:reason ast)
        {:parse-error  ast}
        (let [typ (dang.typechecker/typecheck ast)]
          (if (:type-error typ)
            typ
        ;; TODO: error handling for botched evaluation
            (dang.evaluate/evaluate ast)))))
    (catch Exception e  e)))

;; Calva now launches the alias on jack-in
;; so one can mess with the sesssion state in
;; the repl by requiring the relevant functions
;; usually probably just want to redef server tho
(defn server []
  (let [input (read-line)]
    (println (eval-pcf input))))


(comment (->> nil ;; (java.io.BufferedReader. *in*)
              line-seq
              doall
              vec
              (clojure.string/join "\n")))

(comment
  (nil? (dang.typechecker/typecheck (dang.parser/parse-ast "true")))
  (nil? (dang.typechecker/typecheck (dang.parser/parse-ast "is-zero 1")))
  (nil? (dang.typechecker/typecheck (dang.parser/parse-ast "0 1")))
  (keyword "dang.parser" "nat"))

(comment (->> "let 
        add = 
          fix 
            (\\recurse : Nat -> Nat -> Nat. 
               \\x : Nat. \\y : Nat. 
                 if is-zero x then 
                   y 
                 else 
                   recurse (pred x) (suc y)) in add 3 4"
              dang.parser/parse-ast
              ;;  dang.typechecker/typecheck
              ;; dang.evaluate/evaluate
              ))