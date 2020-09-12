(ns dang.main
  (:require [clojure.string]
            [clojure.repl]
            [dang.parser]
            [dang.typechecker]
            [dang.evaluate]
            [dang.cheeky]))

(defonce tap-atom (atom []))

(defn append-atom [x] (swap! tap-atom conj x))

(defn reset-tap [] (reset! tap-atom []))

(comment (add-tap append-atom)
         (reset-tap)
         @tap-atom)

(def parse dang.parser/parse-ast)

(def typecheck dang.typechecker/typecheck)

(def evaluate dang.evaluate/evaluate)

(defn eval-pcf [str]
  (try
    (let [syntax-str (clojure.string/trim str)
          ast (parse syntax-str)
          typ (typecheck ast)
          res (evaluate ast)]
      (tap> {:in str :parse ast :type typ :out res})
      (if (:reason ast)
        {:parse-error ast}
        (if (:type-error typ) typ res)))
    (catch Exception e e)))

;; Calva now launches the alias on jack-in
;; so one can mess with the sesssion state in
;; the repl by requiring the relevant functions
;; usually probably just want to redef server tho
(defn server []
  (let [input (read-line)]
    (println (eval-pcf input))))


(comment
  (dang.typechecker/typecheck [:app 0 1])
  (->> nil ;; (java.io.BufferedReader. *in*)
       line-seq
       doall
       vec
       (clojure.string/join "\n"))
  (nil? (dang.typechecker/typecheck (dang.parser/parse-ast "true")))
  (nil? (dang.typechecker/typecheck (dang.parser/parse-ast "is-zero 1")))
  (nil? (dang.typechecker/typecheck (dang.parser/parse-ast "0 1")))
  (keyword "dang.parser" "nat")
  (->> "let 
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