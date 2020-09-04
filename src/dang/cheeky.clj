(ns dang.cheeky
  (:require [meander.epsilon :as m]
            [dang.evaluate]
            [clojure.test :as test :refer [is]]))

(def fix (fn [f] (fn [x] ((f (fix f)) x))))

(fix (fn [_] 1))

(defn fixx
  ([f] (f f))
  ([f x] ((f (fixx f)) x)))

(defn- compyl "( ͡° ͜ʖ ͡°)" [expr]
  (m/match expr
    :dang.ast/is-zero 'dang.evaluate/is-zero
    :dang.ast/pred 'dang.evaluate/pred
    :dang.ast/succ 'dang.evaluate/suc

    [:lam (m/symbol _ _ :as ?var) _ ?body]
    `(fn ~[?var] ~(compyl ?body))

    [:app ?fn ?arg]
    `(~(compyl ?fn) ~(compyl ?arg))

    [:fix ?fn]
    `(fix ~(compyl ?fn))

    [:let (m/symbol _ _ :as ?var) ?binding ?body]
    `(let ~[?var (compyl ?binding)] ~(compyl ?body))

    [:if-then-else (m/app compyl (m/pred boolean) ?cond) ?then ?else]
    `(if ~?cond ~(compyl ?then) ~(compyl ?else))

    _ expr))

(defn evaluate [ast]
  (try ((comp eval compyl) ast)
       (catch RuntimeException _  ast)))


(comment
  (resolve 'x)
  (evaluate 'x)
  (evaluate [:app :dang.ast/succ 'x])
  (fixx (fn [_] 1))
  (fixx (fn [rec]
          (fn [x] (if (dang.evaluate/is-zero x)
                    0 (rec (dang.evaluate/pred x))))) 23)
  (is (= 0
         ((fix (fn [rec]
                 (fn [x] (if (dang.evaluate/is-zero x)
                           0 (rec (dang.evaluate/pred x)))))) 23)))

  (def vfix "variadic fix"
    (fn [f] (fn [& args] (apply f (vfix f) args))))

  (def add (fn [recurse] (fn [x] (fn [y] (= 0 x) y ((recurse (- x 1)) (+ 1 y))))))

  (compyl dang.evaluate/fix-realistic)

  (dang.cheeky/fix
   (clojure.core/fn [rec] (clojure.core/fn [bleh] (if (dang.evaluate/is-zero bleh) 0 (rec (dang.evaluate/pred bleh)))))
   2)

  (((fixx add) 2) 3)

  ((vfix (fn [rec x y]
           (if (= y 0) x
               (rec (dang.evaluate/suc x) (dang.evaluate/pred y))))) 35 5)

  (is (= true (compyl true)))
  (is (= '(a b) (compyl '[:app a b])))

  (is (= (eval (compyl '[:app :dang.ast/is-zero 0])) true))

  (is (= true (->> (compyl
                    '[:app
                      [:lam x :dang.ast/nat [:app :dang.ast/is-zero x]]
                      0])
                   eval)))

  (is (= 222 (->> '[:let x 1 [:if-then-else [:app :dang.ast/is-zero x] 111 222]]
                  compyl
                  eval)))

  (is (= 0 (->> dang.evaluate/fix-realistic
                compyl
                eval))))