(ns dang.cheeky
  (:require [meander.epsilon :as m]
            [dang.evaluate]
            [clojure.test :as test :refer [is]]
            [clojure.walk :as walk]))

(def fix (fn [f] (fn [x] ((f (fix f)) x))))



(walk/postwalk #(get {1 'YO} % %) '[1 2 (+ 1 1)])
(walk/postwalk-replace {1 'YO} '[1 2 (+ 1 1)])




(walk/postwalk-replace
 {'rec (second dang.evaluate/fix-realistic)}
 '[:app
   [:fix
    [:lam
     rec
     [:dang.ast/nat :dang.ast/nat]
     [:lam bleh :dang.ast/nat [:if-then-else [:app :dang.ast/is-zero bleh] 0 [:app rec [:app :dang.ast/pred bleh]]]]]]
   2])


(defn fixx
  ([f] (f f))
  ([f x] ((fix f) x)))

(def vfix "variadic fix"
  (fn [f] (fn [& args] (apply f (vfix f) args))))

(def add (fn [recurse] (fn [x] (fn [y] (= 0 x) y ((recurse (- x 1)) (+ 1 y))))))



(((vfix add) 35) 5)
((vfix add) 35 5)

((vfix (fn [rec x y]
         (if (= y 0) x
             (rec (dang.evaluate/suc x) (dang.evaluate/pred y))))) 35 5)

(fix (fn [_] 1))
((fix (fn [rec]
        (fn [x] (if (dang.evaluate/is-zero x)
                  0 (rec (dang.evaluate/pred x)))))) 23)

(fixx (fn [_] 1))
(fixx (fn [rec]
        (fn [x] (if (dang.evaluate/is-zero x)
                  0 (rec (dang.evaluate/pred x))))) 23)
(is (= 0
       ((fix (fn [rec]
               (fn [x] (if (dang.evaluate/is-zero x)
                         0 (rec (dang.evaluate/pred x)))))) 23)))

(defn- compyl "( ͡° ͜ʖ ͡°)" [expr]
  (m/match expr
    :dang.ast/is-zero 'dang.evaluate/is-zero
    :dang.ast/pred 'dang.evaluate/pred
    :dang.ast/succ 'dang.evaluate/suc

    [:lam (m/symbol _ _ :as ?var) _ ?body]
    `(fn ~[?var] ~(compyl ?body))

    ;; [:app [:fix ?fn] ?arg]
    ;; `(fixx ~(compyl ?fn) ~(compyl ?arg))

    [:app ?fn ?arg]
    `(~(compyl ?fn) ~(compyl ?arg))

    [:fix (m/and [:lam ?var _ ?body] ?fn)]
    ;; TODO; crashes, mix in clojure fix
    (compyl (walk/postwalk-replace {?var [:fix ?fn]} ?body))

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

  (compyl [:fix [:lam 'x ::ignore 1]])

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

  (->> dang.evaluate/fix-realistic
       compyl
    ;;    eval
       )
  (is (= 0 (->> dang.evaluate/fix-realistic
                compyl
                ;; eval
                ))))