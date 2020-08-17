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

    [:if-then-else ?cond ?if-true ?if-false]
    (let [cond-type (typecheck-ctx ?cond ctx)
          if-type (typecheck-ctx ?if-true ctx)
          else-type (typecheck-ctx ?if-false ctx)]
      (when
       (and (= cond-type ::boolean)
            (= if-type else-type))
        if-type))

    [:app (m/keyword _ _ :as ?builtin) ?arg]
    (let [is-builtin (builtin? ?builtin)
          arg-nat (= (typecheck-ctx ?arg ctx) ::nat)]
      (when (and is-builtin arg-nat) ::nat))

    [:app ?fn ?arg]
    ;; types are binary trees where branches are arrows
    (let [fn-type (typecheck-ctx ?fn ctx)
          arg-type (safe-head fn-type)
          return-type (safe-second fn-type)]

      (when (= arg-type (typecheck-ctx ?arg ctx)) return-type))


    _other nil))

(defn typecheck [expr] (typecheck-ctx expr {}))

(comment
  (typecheck "foo")
  (typecheck 1232312)
  (typecheck true)
  (typecheck [:if-then-else true false 123])
  (typecheck [:if-then-else 123 false false])
  (typecheck [:if-then-else true false false])
  (typecheck [:if-then-else false 123 123])
  (typecheck [:app :dang.parser/is-zero [:if-then-else false 123 123]])
  (typecheck [:app :dang.parser/is-zero 123]))