(ns dang.db
  (:require [datahike.api :as d]
            [clojure.test :as test]))

(def fix-realistic
  '[:app
    [:fix
     [:lam
      rec
      [:dang.ast/nat :dang.ast/nat]
      [:lam bleh :dang.ast/nat
       [:if-then-else [:app :dang.ast/is-zero bleh] 0
        [:app rec [:app :dang.ast/pred bleh]]]]]]
    2])

(def program
  {:dang/program
   {:app/fn
    {:fix/fn
     {:lam/var 'rec
      :lam/type {:type/head :dang.ast/nat :type/tail :dang.ast/nat}
      :lam/body
      {:lam/var 'bleh
       :lam/type {:type/head :dang.ast/nat}
       :lam/body
       {:if/cond {:app/fn :dang.ast/is-zero
                  :app/arg {:var/ref 'bleh}}
        :if/then {:prim/nat 0}
        :if/else {:app/fn {:var/ref 'rec}
                  :app/arg {:app/fn :dang.ast/pred
                            :app/arg {:var/ref 'bleh}}}}}}}}
   :app/arg {:prim/nat 2}})

(def cfg {:store {:backend :mem :id "my-db"}})

(d/create-database cfg)

(def conn (d/connect cfg))


(defn ident
  ([id] {:db/ident id})
  ([id vt] {:db/ident id
            :db/valueType vt
            :db/cardinality :db.cardinality/one})
  ([id vt card] {:db/ident id
                 :db/valueType vt
                 :db/cardinality card
                ;;  :db/isComponent (not= id :dang.program)
                 }))

(def schema
  (mapv (fn [args] (apply ident args))
        [[:dang/program :db.type/ref :db.cardinality/one]

         [:app/fn :db.type/ref :db.cardinality/one]
         [:app/arg :db.type/ref :db.cardinality/one]

         [:fix/fn :db.type/ref :db.cardinality/one]

         [:lam/var :db.type/symbol :db.cardinality/one]
         [:lam/type :db.type/ref :db.cardinality/one]
         [:lam/body :db.type/ref]

         [:if/cond :db.type/ref]
         [:if/then :db.type/ref]
         [:if/else :db.type/ref]

         [:type/head :db.type/ref]
         [:type/tail :db.type/ref]
         [:dang.ast/nat]
         [:dang.ast/boolean]

         [:dang.ast/is-zero]
         [:dang.ast/pred]
         [:dang.ast/succ]

         [:var/ref :db.type/symbol]
         [:prim/nat :db.type/number]
         [:prim/bool :db.type/boolean]]))

(d/transact conn schema)

(d/transact conn [program])

(defonce after-program (d/db conn))

(d/q '[:find ?lambda
       :in $ ?sym
       :where
       [?lambda :lam/var ?sym]]
     after-program
     'bleh)

(->> (d/q '[:find (pull ?ref [:app/_fn])
            :in $ ?sym
            :where
            [?ref :var/ref ?sym]]
          after-program
          'rec)
     first
     first
     :app/_fn)

(d/q '[:find (pull ?prog [*])
       :where [?prog :dang/program _]] after-program)

(defn ast->tx [_expr])

(test/is (= program (ast->tx fix-realistic)))