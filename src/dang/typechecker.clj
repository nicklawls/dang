(ns dang.typechecker
  (:require
   [meander.epsilon :as m]))

(defn- builtin? [x]
  (boolean (#{:dang.parser/succ
              :dang.parser/pred
              :dang.parser/is-zero} x)))

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
    (m/pred number?) ::nat

    (m/pred boolean?) ::boolean

    [:if-then-else
     (m/app #(typecheck-ctx %1 ctx) ::boolean)
     (m/app #(typecheck-ctx %1 ctx) ?body-type)
     (m/app #(typecheck-ctx %1 ctx) ?body-type)]
    ?body-type

    (m/keyword _ _ :as (m/pred builtin?))
    [::nat ::nat]

    [:app
     (m/app #(typecheck-ctx %1 ctx) [?arg-type ?return-type])
     (m/app #(typecheck-ctx %1 ctx) ?arg-type)]
    ;; types are binary trees where branches are arrows
    ?return-type


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
  (typecheck [:app :dang.parser/pred true])
  (typecheck [:app :dang.parser/is-zero [:if-then-else false 123 123]])
  (typecheck [:app :dang.parser/is-zero 123])
  (typecheck [:app :dang.parser/pred 33333])
  (typecheck [:app :dang.parser/succ 33333]))