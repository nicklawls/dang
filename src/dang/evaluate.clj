(ns dang.evaluate
  (:require
   [meander.epsilon :as m]
   [clojure.set :as set]))

;; Notes from wrasslin with desugar-fix
; fix(f) = x if f(x) = x
; fix(f) = f(fix(f)) -> direct implementation recurses forever
; fix(f) := let x = f(x) in x
; some way to hide another fix under a lambda till it's needed
; hide it in one of the recursive conditions of the function being applied
; (fix f) f === (\g. )
;; guess i have to do  (fix f) a = (f (fix f)) a, have the arg in hand? nope, infinities
;; with lazy y + separate fix and app fix === converged on big gnarly
;; lazy y with only fix === infinities
;; HAD :fix AT THE HEAD LOL
;; yup still infinite
;; y not try good ol z again?
;; munged version from the article is equivalent to 
;; ones I've tried

(def is-zero #(= 0 %))


(def suc #(+ 1 %))


(def pred #(max 0 (- % 1)))


(defn- free-vars [expr]
  (m/match expr
    (m/symbol _ _ :as ?sym) #{?sym}

    [:lam ?var _ ?body]
    (set/difference (free-vars ?body)  #{?var})

    ;; generalize the rule for app to :if-then-else and :fix    
    ;; could do as in forcibly-update, but memory variables
    ;; are siccc
    [(m/keyword _ _) !xs ...]
    (apply set/union (mapv free-vars !xs))

    _ #{}))


;; might be more subtle, see reference impl
;; actually, seems equivalent, they just don't 
;; recurse inside the first lam arg, do another if directly
(defn- forcibly-replace [var new-sym body]
  (m/match body
    (m/let [?var var] ?var)
    new-sym

    (m/let [?var var] [:lam ?var _ ?body])
    [:lam new-sym ::ignore (forcibly-replace var new-sym ?body)]

    (m/pred vector?)
    (mapv #(forcibly-replace var new-sym %) body)
    _
    body))

;; Substitution
;; e's normal form terms: ie e terms or lambdas applied to terms
;; x,y terms

;; term match
;; x{e/x} = e

;; term mismatch
;; y{e/x} = y 

;; application
;; (e1 e2){e/x} = (e1{e/x} e2{e/x})

;; lam arg name match
;; (fun x -> e'){e/x} = fun x -> e' 

;; lam arg name mismatch, no FV
;; (fun y -> e'){e/x} = fun y -> e'{e/x} if y is not in FV(e)

;; lam arg name mismatch, FV
;; (fun y -> e'){e/x} = (fun fresh -> (e'[fresh/y]){e/x} ) y is in FV(e)
;; 
(defn- substitute
  "\"a correct definition of it eluded mathemeticians for centuries\""
  [var substitution expr]
  (m/match [expr var]
    [(m/symbol _ _ :as ?var-name) ?var-name] substitution
    [(m/symbol _ _ :as ?var-name) _] expr

    ;; oh snap, this is lexical scope right here
    ;; skip the outer variable, whatever evaluation procedure
    ;; will just apply the evaluated arg next?
    [[:lam ?var ?type ?body] ?var] ;; (fn [x] (x + x)) x
    expr

    ;; if we have a free var clash, generate a fresh var
    ;; and replace the clashing var with it, in both lambda and it's body
    ;; then substitute as usual
    ;; otherwise substitute as usual
    ;;       
    [[:lam ?lam-var ?type ?body] ?var]
    (if
     (contains? (free-vars ?body) ?lam-var)

      (let [new-sym (gensym "subst")
            new-body
            (forcibly-replace ?lam-var new-sym ?body)]
        [:lam new-sym ?type (substitute ?var substitution new-body)]) ;; (fn [y] ())

      ;; double-lam hitting here
      [:lam ?lam-var ?type (substitute ?var substitution ?body)])

    [[:app ?fn ?arg] _]
    [:app
     (substitute var substitution ?fn)
     (substitute var substitution ?arg)]

    [[:let ?name ?binding ?body] _]
    (substitute
     var
     substitution [:app [:lam ?name ::ignore ?body] ?binding])

    ;; fix and if-then-else don't need anything extra
    [(m/pred vector? ?vec) _]
    (mapv #(substitute var substitution %) ?vec)

    _ expr))

;; whnf(x)    = x
;; whnf(\x.e) = (\x.e)
;; 
;; whnf(e1) = (\x.e)  &  whnf(e{e2/x}) = e'
;; ---------------------------------------
;;            whnf((e1 e2)) = e'
;;            
;; whnf(e1) = e1' /= (\x.e)
;; ------------------------
;; whnf((e1 e2)) = (e1' e2)
;; 
;; whnf(if(e1 e2 e3))
;; 

(defn whnf-next [expr]
  (m/match expr
    ;; prims are prims

    ;; don't go under abstractions
    [:lam ?var ?type ?body]
    [:lam ?var ?type ?body]

    [:app :dang.ast/is-zero (m/pred number? ?n)]
    (is-zero ?n)

    [:app :dang.ast/succ (m/pred number? ?n)]
    (suc ?n)

    [:app :dang.ast/pred (m/pred number? ?n)]
    (pred ?n)

    [:app (m/keyword _ _ :as ?builtin) ?exp]
    [:app ?builtin (whnf-next ?exp)]

    ;; find redxes
    [:app (m/and ?fn [:lam ?var ?type ?body] (m/app whnf-next ?fn))
     (m/and ?arg (m/app whnf-next ?arg))]
    (substitute ?var ?arg ?body)

    ;; function is not whnf, arg unknown
    [:app ?fn ?arg]
    [:app (whnf-next ?fn) ?arg]

    ;; fixy versions
    [:app
     [:fix (m/and ?fn [:lam ?name _ ?body] (m/app whnf-next ?fn))]
     (m/and ?arg (m/app whnf-next ?arg))]
    [:app (substitute ?name [:fix ?fn] ?body) ?arg]

    [:app [:fix ?fn] ?arg]
    [:app [:fix (whnf-next ?fn)] ?arg]

    ;; fix outside of app
    [:fix (m/and ?fn [:lam ?name _ ?body] (m/app whnf-next ?fn))]
    (substitute ?name [:fix ?fn] ?body)
    [:fix ?fn]
    [:fix (whnf-next ?fn)]

    [:let ?name ?binding ?body]
    [:app [:lam ?name ::ignore ?body] ?binding]

    [:if-then-else
     (m/pred boolean? ?cond) ?if-body ?else-body]
    (if ?cond ?if-body ?else-body)

    [:if-then-else ?cond ?then ?else]
    [:if-then-else (whnf-next ?cond) ?then ?else]

    _ expr))

;; evaluate
;; 
;; Normal order reduction to normal form
;; Leftmost outermost redex first
;; 
;; eval(x) = x
;; 
;;     eval(e) = e'
;; ----------------------
;; eval((\x.e)) = (\x.e')
;; 
;; whnf(e1) = (\x.e)  eval(e{e2/x}) = e'
;; ------------------------------------
;;          eval((e1 e2)) = e'
;; 
;; whnf(e1) /= (\x.e)  eval(e'1) = e''1  eval(e2) = e'2
;; ----------------------------------------------------
;;             eval((e1 e2)) = (e''1 e'2)

(defn- eval-next ""
  [expr]
  (m/match expr
    (m/symbol _ _ :as ?sym)
    ?sym

    [:lam ?var-name ?type ?body]
    [:lam ?var-name ?type (eval-next ?body)] ;; unsure if eval under lambda

    [:app :dang.ast/is-zero (m/pred number? ?n)]
    (is-zero ?n)

    [:app :dang.ast/succ (m/pred number? ?n)]
    (suc ?n)

    [:app :dang.ast/pred (m/pred number? ?n)]
    (pred ?n)

    [:app (m/keyword _ _ :as ?builtin) ?exp]
    [:app ?builtin (eval-next ?exp)]

    ;; want reduce fn to a whnf term one step at a time
    ;; then reduce arg to a nf term one step at a time
    ;; sub only when evaluation rule 3 is true do we substitute
    [:app
     (m/and ?fn [:lam ?name _ ?body] (m/app whnf-next ?fn))
     (m/and ?arg (m/app eval-next ?arg))]
    (substitute ?name ?arg ?body)
    ;; lambda is whnf, arg is not nf
    [:app (m/and ?fn [:lam _ _ _] (m/app whnf-next ?fn)) ?arg]
    [:app ?fn (eval-next ?arg)]
    ;; lambda is not whnf, arg unknown
    [:app ?fn ?arg]
    [:app (eval-next ?fn) ?arg]

    ;; fix exprs substitute themselves for the recursive argument
    [:fix (m/and ?fn [:lam ?name _ ?body] (m/app whnf-next ?fn))]
    (substitute ?name [:fix ?fn] ?body)
    [:fix ?fn]
    [:fix (eval-next ?fn)]

    [:let ?name ?binding ?body]
    [:app [:lam ?name ::ignore ?body] ?binding]

    [:if-then-else
     (m/pred boolean? ?cond) ?if-body ?else-body]
    (if ?cond ?if-body ?else-body)

    [:if-then-else ?cond ?then ?else]
    [:if-then-else (eval-next ?cond) ?then ?else]

    _ expr))

(defn evaluate [expr]
  (let [next (eval-next expr)]
    (if (= next expr) expr (evaluate next))))

;;                         TESTING AREA

(def fix-realistic
  '[:app
    [:fix
     [:lam
      rec
      [:dang.ast/nat :dang.ast/nat]
      [:lam bleh :dang.ast/nat
       [:if-then-else
        [:app :dang.ast/is-zero bleh]
        0
        [:app rec [:app :dang.ast/pred bleh]]]]]]
    2])

(comment (evaluate fix-realistic))

(comment
  (->> fix-realistic
       eval-next
       eval-next ;; substitute 2
       eval-next ;; eval condition)
       eval-next ;; discharge then branch
       eval-next ;; substitute recursive fix arg
       eval-next ;; eval pred 2
       eval-next ;; sub 1
       eval-next ;; condition
       eval-next ;; discharge then
       eval-next ;; fix recurse
       eval-next ;; pred 1
       eval-next ;; sub 0
       eval-next ;; condition
       eval-next ;; discarge else
       ))

;; ((fix (\rec x. if (zero x) then 0 else rec (pred x))) 2) 
;; leftost outermost = "app" (fix lambda) lambda

;; ((\x. if (zero x) then 0 else ((fix (\rec x. ...)) (pred x)) 2)
;; leftmost outermost = sub 2 x

;; if (zero 2) then 0 else ((fix (\rec x. ...)) (pred 2))
;; reduce if cond

;; if false then 0 else ((fix (\rec x. ...)) (pred 2))
;; branch
;; 
;; ((fix (\rex x. ...)) (pred 2))
;; app (fix f)f

(comment
  (evaluate false)
  (evaluate 2)
  (evaluate [:app :dang.ast/is-zero 0])
  (evaluate [:app :dang.ast/succ 1])
  (evaluate [:if-then-else false 1 0])
  (evaluate [:app [:lam 'foo :dang.ast/nat 'foo] 0])
  (evaluate [:let 'foo 32 [:app :dang.ast/succ 'foo]])
  (evaluate [:app [:lam 'foo ::ignore 32] true])
  (evaluate [:app [:lam 'foo ::ignore 32] true])
  (evaluate [:fix [:lam 'x :dang.ast/nat 1]])
  (->> [:fix :dang.ast/succ 3]
       eval-next)
  (->> [:fix [:lam 'x :dang.ast/nat 1]]
       eval-next))