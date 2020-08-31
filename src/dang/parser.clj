(ns dang.parser
  (:require [clojure.string]
            [instaparse.core :as insta]
            [clojure.edn :as edn]))

(def parser
  (insta/parser
   "expr = bool-lit | nat-lit | builtin | var | 
           paren-expr | (if-then-else / app / fix / let) | lam;
    
    app = expr <whitespace> expr;
    
    paren-expr = <'(' [whitespace]> expr <[whitespace] ')'>
    
    fix = <'fix' whitespace> expr;
    
    lam = <'\\\\' [whitespace]> var 
          <[whitespace] ':' [whitespace]> type 
          <[whitespace] '.' [whitespace]> expr;
    
    type = type-lit | arrow | paren-arrow;
    
    arrow = type <[whitespace] '->' [whitespace]> type;
    
    paren-arrow = <'('[whitespace]> arrow <[whitespace]')'>;
    
    type-lit = 'Nat' | 'Bool';
    
    let = <'let' whitespace> var 
          <whitespace '=' whitespace> expr 
          <whitespace 'in' whitespace> expr;
    
    if-then-else = <'if' whitespace> expr 
                   <whitespace 'then' whitespace> expr 
                   <whitespace 'else' whitespace> expr;
    
    var = !(keyword whitespace) &lowercase-first #'[a-zA-Z0-9\\-_]+';
    lowercase-first = #'[a-z]';
    keyword = 'let' | 'in' | 'fix' | 'if' | 'then' | 'else' | bool-lit | builtin;
    
    bool-lit = 'true' | 'false';
    nat-lit = #'[0-9]+';
    
    builtin = 'suc' | 'pred' | 'is-zero';
    
    whitespace = #'(\\s|\\n)+';
    "))

(defn parse-ast
  "Run the parser and apply transforms to keep the AST minimal

   TODO: spec the shape of the AST"
  [input]
  (->>
   (parser input)
   (insta/transform
    {;; literals are the corresponding clj primitives
     :bool-lit edn/read-string
     :nat-lit edn/read-string

     ;; builtins are keywords
     :builtin #(if (= "suc" %) :dang.ast/succ (keyword "dang.ast" %))

     ;; main expr nodes just wrap other combos,
     ;; extra tag is redundant
     :expr identity
     :paren-expr identity
     :app-arg identity

     ;; binding names are symbols, because we got em
     :var symbol

     ;; type decls are either a single type keyword
     ;; or a binary tree of type keywords
     :type identity
     :arrow vector
     :paren-arrow identity
     :type-lit #(keyword
                 "dang.ast"
                 (clojure.string/lower-case %1))

     ;; un-messed-with are
     ;; [:fix <fix-val>]
     ;; [:let <name> <binding> <body>]
     ;; [:lam <name> <type> <body>]
     ;; [:app <fn> <arg>]
     ;; [:if-then-else <cond> <if-true> <if-false>]
     })))

(comment
  (parse-ast "true")
  (parse-ast "false")
  (parse-ast "123")
  (parse-ast "123 
        456")
  (parse-ast "succ 123 is-zero hi-mom__123_bleh")
  (parse-ast "(succ 123) is-zero hi-mom__123_bleh")
  (parse-ast "if 32 then 52 else 42")
  (parse-ast "let flEEEgle = 145 in succ flEEgle")
  (parse-ast "\\x : Nat. x")
  (parse-ast "fix (\\x : Nat -> Nat. 1)")
  (parse-ast "fix (\\x : Nat -> (Nat -> Bool). 1)")
  (parse-ast "fix (\\x : Nat -> ((Bool -> Nat) -> Bool). 1)")
  (parse-ast "(recurse (pred x)) (suc y)")
  (parse-ast "recurse (pred x) (suc y)")
  ;; fixing the above with [app = expr <ws> expr] broke the following
  (->> "if is-zero x y then 0 else rec (pred x)"
       (insta/parses parser))
  ;; the desired result was at the bottom, added (if-then-else / app)
  (->> "fix (\\rec : Nat -> Nat. \\x : Nat. if is-zero x then 0 else rec (pred x)) 2"
       parse-ast)
  ;; then ordered fix and let to get this worked out
  (->> "let add = fix (\\rec : Nat -> Nat. \\x : Nat. \\y : Nat. 
        if is-zero x then y else rec (pred x) (suc y)) in add 3 4"
       parse-ast)
  ;; resulting parse is [:app [:let ...] 4]
  ;; not as clean as [;let ... [:app [:app]]], but works!
  )