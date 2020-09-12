(ns dang.cheeky
  (:require [meander.epsilon :as m]
            [dang.evaluate]
            [clojure.pprint :as p]
            [clojure.test :as test :refer [is]]
            [clojure.walk :as walk]))

(defn- compyl "( ͡° ͜ʖ ͡°)" [expr]
  (m/match expr
    :dang.ast/is-zero 'zero?
    :dang.ast/pred '(fn [x] (max 0 (dec x)))
    :dang.ast/succ 'inc

    [:lam (m/symbol _ _ :as ?var) _ ?body]
    `(fn ~[?var] ~(compyl ?body))

    [:app ?fn ?arg]
    `(~(compyl ?fn) ~(compyl ?arg))

    ;; two fix cases: with and without a nested lambda
    ;; not convinced it works for expressions with bodies that evaluate
    ;; to functions, vs expressions with syntax that hit the first case
    [:fix [:lam ?var _ [:lam ?var2 _ ?inner-body]]]
    (let [label (gensym "fn-name")]
      `(fn ~label [~?var2]
         ~(walk/postwalk-replace {?var label} (compyl ?inner-body))))

    [:fix [:lam ?var _ ?body]]
    (let [label (gensym "fn-name")]
      `(let [~label ~(walk/postwalk-replace {?var label}
                                            (compyl ?body))]
         ~label))

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

  ((fn [gend]
     (loop [bleh gend]
       (if (zero? bleh) 0
           (recur (dec bleh))))) 2)

;;   (let [neg (fn [x] (if (= 0 x) 0 (neg (- x 1))))]  (neg 32))

  ((fn meh [bleh]
     (println bleh)
     (if (zero? bleh)
       0 (meh (dec bleh)))) 2)

  (->> [:fix [:lam 'x ::ignore [:app :dang.ast/succ 1]]]
       compyl
       eval
    ;;    second
    ;;    (#(nth % 3))
    ;;    compyl
    ;;    (walk/postwalk-replace {'x 'my-name})
    ;;    ((fn [arg] `(let ['myname ~arg] ~arg)))
    ;;    eval
       )

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
  (->> dang.evaluate/fix-realistic
       compyl
       eval)

  ((fn [x] (max 0 (dec x))) 1)

  (is (= 0 (->> dang.evaluate/fix-realistic
                compyl
                eval))))