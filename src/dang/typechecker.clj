(ns dang.typechecker
  (:require
   [meander.epsilon :as m]))

(defn- builtin? [x]
  (boolean (#{:dang.ast/succ
              :dang.ast/pred
              :dang.ast/is-zero} x)))

;; some things never change
(defn- safe-head [coll]
  (try (first coll) (catch Exception _e nil)))

(defn- safe-second [coll]
  (try (second coll) (catch Exception _e nil)))

(comment
  (safe-head [[123]])
  (safe-second 'jdfkdf))

;; check symbols against context
;; make sure symbol names never clash/ always shadow
(defn typecheck-ctx [expr ctx]
  (m/match expr
    (m/pred number?) :dang.ast/nat

    (m/pred boolean?) :dang.ast/boolean

    [:if-then-else
     (m/app #(typecheck-ctx %1 ctx) :dang.ast/boolean)
     (m/app #(typecheck-ctx %1 ctx) (m/not nil) ?body-type)
     (m/app #(typecheck-ctx %1 ctx) ?body-type)]
    ?body-type

    (m/keyword _ _ :as (m/pred builtin?))
    [:dang.ast/nat :dang.ast/nat]

    (m/symbol _ _ :as (m/app ctx (m/not nil) ?var-type))
    ?var-type

    [:let ?name ?binding ?body]
    (typecheck-ctx
     ?body
     (assoc ctx ?name (typecheck-ctx ?binding ctx)))

    [:app
     (m/app #(typecheck-ctx %1 ctx)
            [?arg-type (m/and (m/not nil) ?return-type)])
     (m/app #(typecheck-ctx %1 ctx) ?arg-type)]
    ;; types are binary trees where branches are arrows
    ?return-type

    [:lam ?name ?var-type
     (m/app #(typecheck-ctx %1 (assoc ctx ?name ?var-type))
            (m/not nil)
            ?body-type)]
    [?var-type ?body-type]

    [:fix (m/app #(typecheck-ctx %1 ctx) [?fixtype ?fixtype])]
    ?fixtype

    _other nil))

(defn typecheck [expr] (typecheck-ctx expr {}))

(comment ('himom (assoc {} 'himom :nat)))
(comment
  (typecheck "foo")
  (typecheck 1232312)
  (typecheck true)
  (typecheck [:if-then-else true false 123])
  (typecheck [:if-then-else 123 false false])
  (typecheck [:if-then-else true false false])
  (typecheck [:if-then-else false 123 123])
  (typecheck [:app :dang.ast/pred true])
  (typecheck [:app :dang.ast/is-zero [:if-then-else false 123 123]])
  (typecheck [:app :dang.ast/is-zero 123])
  (typecheck [:app :dang.ast/pred 33333])
  (typecheck [:app :dang.ast/succ 33333])
  (typecheck-ctx 'foobar {'foobar ::boolean})
  (typecheck-ctx 'bazzle {'foobar ::boolean})
  (typecheck [:let 'foobar 32 [:app :dang.ast/succ 'foobar]])
  (typecheck [:let 'foobar 32 [:let 'foobar true 'foobar]])
  (typecheck [:lam 'foobar :dang.ast/boolean [:if-then-else 'foobar 32 33]])
  (typecheck [:fix :dang.ast/succ]))