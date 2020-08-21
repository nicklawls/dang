(ns dang.parser
  (:require [clojure.string]
            [instaparse.core :as insta]))

(def parser
  (insta/parser
   "expr = app | app-arg | if-then-else | let | lam
    
    app = app-arg <whitespace> expr
    app-arg = paren-expr | fix | bool-lit | nat-lit | builtin | var
    paren-expr = <'(' [whitespace]> expr <[whitespace] ')'>
    
    lam = <'\\\\' [whitespace]> var <[whitespace] ':' [whitespace]> type <[whitespace] '.' [whitespace]> expr
    
    type = type-lit | arrow | paren-arrow
    
    arrow = type <[whitespace] '->' [whitespace]> type
    
    paren-arrow = <'('[whitespace]> arrow <[whitespace]')'>
    
    type-lit = 'Nat' | 'Bool'
    
    fix = <'fix' whitespace> app-arg
    
    let = <'let' whitespace> var <whitespace '=' whitespace> expr <whitespace 'in' whitespace> expr
    
    if-then-else = <'if' whitespace> expr <whitespace 'then' whitespace> expr <whitespace 'else' whitespace> expr
    
    var = !(keyword whitespace) &lowercase-first #'[a-zA-Z0-9\\-_]+'
    lowercase-first = #'[a-z]'
    keyword = 'let' | 'in' | 'fix' | 'if' | 'then' | 'else' | bool-lit | builtin
    
    bool-lit = 'true' | 'false'
    nat-lit = #'[0-9]+'
    
    whitespace = #'(\\s|\\n)+'
    builtin = 'suc' | 'pred' | 'is-zero'
    "))

(defn parse-ast
  "Run the parser and apply transforms to keep the AST minimal

   TODO: spec the shape of the AST"
  [input]
  (->>
   (parser input)
   (insta/transform
    {;; literals are the corresponding clj primitives
     :bool-lit read-string
     :nat-lit read-string

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
  (parse-ast "fix (\\x : Nat -> ((Bool -> Nat) -> Bool). 1)"))