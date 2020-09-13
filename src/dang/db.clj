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
                            :app/arg {:var/ref 'bleh}}}}}}}
    :app/arg {:prim/nat 2}}})

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

         [:let/var :db.type/symbol]
         [:let/binding :db.type/ref]
         [:let/body :db.type/ref]

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
     @conn
     'bleh)

(->> (d/q '[:find (pull ?ref [:app/_fn])
            :in $ ?sym
            :where
            [?ref :var/ref ?sym]]
          @conn
          'rec)
     first
     first
     :app/_fn)

(d/q '[:find (pull ?prog [*])
       :where [?prog :dang/program _]] @conn)
;; => ([{:db/id 21, :app/arg #:db{:id 36}, :dang/program #:db{:id 22}}])

;; oh snap, accidentally had :app/arg on the program, not its application
;; child. retract it from the program entity
;; then add it to the program's :dang/program (baddly named attr)

(def bad-ent (d/entity @conn 21))

(def arg-id (:db/id (:app/arg bad-ent)))

(d/transact
 conn
 [[:db/retract 21 :app/arg arg-id]
  [:db/add (:db/id (:dang/program bad-ent)) :app/arg arg-id]])

;; try again
;; 
(d/q '[:find (pull ?prog [*])
       :where [?prog :dang/program _]] @conn)
;; => ([{:db/id 21, :dang/program #:db{:id 22}}])

(d/q '[:find (pull ?node [*])
       :where [_ :dang/program ?node]] @conn)
;; => ([{:db/id 22, :app/arg #:db{:id 36}, :app/fn #:db{:id 23}}])


;; TODO: types of queries (and indices) depend upon the language, and
;; the analyses we want to do
;; 
;; A language/environment for live-programmable web apps and dashboards
;; * register data soruces from various web services and APIs
;; * Markdown on crack
;; * make disgustingly dense and interesting links on the fly and
;; * send them to friends and family.
;; * language for philosophy and love.
;; 
;; Analyses 
;; * compiler optimizations (Martin odersky's compiler as database)
;; * AOT virtual dom stuff (a'la svelte, whatever that newer thing is)
;; * editor features
;; * dataflow analysis
;; * incremental computation
;; * compile-to-cloud
;; * code/data generation (from types/schemas)
;; 
;; Language Features
;; * programs expressed as time-varying functions of the EAVT
;;   database that is simultaneously
;;   the codebase, database, and deployment registry,
;;   and where execution traces can optionally be stored
;;   * some of those functions are interpreted with datomic
;;     transaction fn semantics
;; * algebraic subtyping?
;; * dependent types?
;; * liquid/quotient types?
;; * metaprogramming?
;;   * even the haskellers can't stop talking about staging
;; * algebraic effects?
;; * based on term-rewriting calculi?
;;   * not sure what practical benefits
;;   * may be a way to unify dsl metaprogramming and algebraic effects?
;; * hot reloading
;;   * live editing and deployment
;;   * diff and patch against a constantly running system
;; * typed holes
;; * text buffer saving as interface to semantic edit actions
;; * simple semantic version control
;;   * unison-like hashing or mechanized Rich Hickey rules
;; * small core, most features in userland
;; 
;; example:
;; color the let keyword dark blue, whole expresssion blue background
;; color app red w/red background
;; format: let
'(search
  [:let ?name ?binding ?body]
  (row {:bg blue}
       [(el {:color darkBlue} "let")
        ()])) ;; => 

;; Will probably be defined as instaparse transformer
;; want the full syntax tree with spans and line/column info
;; current parser forgets certain nodes and loses line info 
;; on numbers, booleans, and symbols
(defn ast->tx [_expr])

(test/is (= program (ast->tx fix-realistic)))