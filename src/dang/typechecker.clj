(ns dang.typechecker
  (:require
   [meander.epsilon :as m]))

(defn- typecheck-ctx
  "inner helper that takes a context map and threads it through"
  [expr ctx]
  (let [;; Helpers

        ;; typecheck with the current context
        check #(typecheck-ctx %1 ctx)

        ;; check with additional binding
        check-with (fn [k v] #(typecheck-ctx %1 (assoc ctx k v)))]

    (m/match expr
      ;; primitives are primitive types
      (m/pred number?)
      :dang.ast/nat

      (m/pred boolean?)
      :dang.ast/boolean

      ;; condition must be boolean
      ;; then and else branches must match
      [:if-then-else
       (m/app check :dang.ast/boolean)
       (m/app check (m/not nil) ?body-type)
       (m/app check ?body-type)]
      ?body-type

      ;; keywords are builtins
      ;; is-zero is Nat -> Bool
      :dang.ast/is-zero
      [:dang.ast/nat :dang.ast/boolean]
      ;; succ/pred are Nat -> Nat 
      (m/keyword "dang.ast" (m/or "succ" "pred"))
      [:dang.ast/nat :dang.ast/nat]

      ;; symbols are vars and should be looked up in ctx
      ;; note that in clojure (map key) === (get map key)
      (m/symbol _ _ :as (m/app ctx (m/not nil) ?var-type))
      ?var-type

      ;; check the binding
      ;; check the body with binding and its type in context
      [:let ?name (m/app check (m/not nil) ?binding-type)
       (m/app (check-with ?name ?binding-type) (m/not nil) ?body-type)]
      ?body-type

      ;; check the fn, split it into argument and return types
      ;; check the arg type
      ;; if expected and observed argument types line up, return return type
      [:app
       (m/app check [?arg-type (m/and (m/not nil) ?return-type)])
       (m/app check ?arg-type)]
      ?return-type

      ;; add arument and its type to context, check the body
      ;; result is a function from var's type to body's type
      [:lam ?name ?var-type
       (m/app (check-with ?name ?var-type) (m/not nil) ?body-type)]
      [?var-type ?body-type]

      ;; All I remember is fix :: (a -> a) -> a
      [:fix (m/app check [?fixtype ?fixtype])]
      ?fixtype

      ;; TODO: could return error maps and forward them
      ;; no match? type error
      _otherwise nil)))

(defn typecheck
  "Takes arbitrary expr 
   
   If sucessful returns nat, boolean, or a binary tree of types representing a function type
   
   If failure returns nil
   "
  [expr] (typecheck-ctx expr {}))

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
  (typecheck-ctx 'foobar {'foobar :dang.ast/boolean})
  (typecheck-ctx 'bazzle {'foobar :dang.ast/boolean})
  (typecheck [:let 'foobar 32 [:app :dang.ast/succ 'foobar]])
  (typecheck [:let 'foobar 32 [:let 'foobar true 'foobar]])
  (typecheck [:lam 'foobar :dang.ast/boolean [:if-then-else 'foobar 32 33]])
  (typecheck [:fix :dang.ast/succ]))