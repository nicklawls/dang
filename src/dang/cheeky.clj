(ns dang.cheeky
  (:require [meander.epsilon :as m]
            [dang.evaluate]
            [clojure.pprint :as p]
            [clojure.test :as test :refer [is]]
            [clojure.walk :as walk]))

(defn- compyl "( ͡° ͜ʖ ͡°)" [expr]
  (m/match expr
    :dang.ast/is-zero 'dang.evaluate/is-zero
    :dang.ast/pred 'dang.evaluate/pred
    :dang.ast/succ 'dang.evaluate/suc

    [:lam (m/symbol _ _ :as ?var) _ ?body]
    `(fn ~[?var] ~(compyl ?body))

    [:app ?fn ?arg]
    `(~(compyl ?fn) ~(compyl ?arg))

    [:fix [:lam ?var _ ?body]]
    ;; TODO still crashes on last example
    ;; "can recur from tail position"
    ;; knowing what I know now, can prob replace
    ;; recur with a reference to the code of the 
    ;; recursive function
    (walk/postwalk-replace {?var 'recur} (compyl ?body))

    [:let (m/symbol _ _ :as ?var) ?binding ?body]
    `(let ~[?var (compyl ?binding)] ~(compyl ?body))

    [:if-then-else (m/app compyl (m/pred boolean) ?cond) ?then ?else]
    `(if ~?cond ~(compyl ?then) ~(compyl ?else))

    _ expr))

(defn evaluate [ast]
  (try ((comp eval compyl) ast)
       (catch RuntimeException e
         [e (p/p (compyl ast))])))


(comment
  (resolve 'x)
  (evaluate 'x)
  (evaluate [:app :dang.ast/succ 'x])

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

  (evaluate dang.evaluate/fix-realistic)
  ;; needs to handle fix on a -> a funcitons
  (evaluate [:fix [:lam 'x ::ignore 1]])
  ((fn named [] 1))

  (loop [_x (fn [_x] 1)] 1)

  (loop [] 1)

  ((fn [gend]
     (loop [bleh gend]
       (if (dang.evaluate/is-zero bleh) 0
           (recur (dang.evaluate/pred bleh))))) 2)

  ((fn [bleh]
     (println bleh)
     (if (dang.evaluate/is-zero bleh)
       0 (recur (dang.evaluate/pred bleh)))) 2)

  (->> [:fix [:lam 'x ::ignore [:app :dang.ast/succ 1]]]
       second
       (#(nth % 3))
       compyl
       (walk/postwalk-replace {'x 'recur})
    ;;    ((fn [arg] `(~arg)))
    ;;    eval
       )


  ;; and (a -> a) -> a -> a functions
  (->> dang.evaluate/fix-realistic
       second
       second
       (#(nth % 3))
       compyl
       (walk/postwalk-replace {'rec 'recur})
    ;;    ((fn [arg] `(~arg 2)))
    ;;    eval
       )
  (is (= 0 (->> dang.evaluate/fix-realistic
                compyl
                eval))))