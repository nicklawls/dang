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
    builtin = 'succ' | 'pred' | 'is-zero'
    "))

(defn parse-ast
  "Run the parser and apply transforms
   
   Probably should save the transform step for meander"
  [input]
  (->>
   (parser input)
   (insta/transform
    {:bool-lit read-string
     :nat-lit read-string
     :expr identity
     :app-arg identity
     :var symbol
     :type vector
     :arrow (comp vec concat)
     :paren-arrow identity
     :paren-expr identity
     :builtin #(keyword (str *ns*) %1)
     :type-lit #(keyword
                 (str *ns*)
                 (clojure.string/lower-case %1))})))

(comment
  (parse-ast "true")
  (parse-ast "false")
  (parse-ast "123")
  (parse-ast "123 
        456")
  (parse-ast "(succ 123) is-zero hi-mom__123_bleh")
  (parse-ast "if 32 then 52 else 42")
  (parse-ast "let flEEEgle = 145 in succ flEEgle")
  (parse-ast "\\x : Nat. x")
  (parse-ast "fix (\\x : Nat -> Nat. 1)")
  (parse-ast "fix (\\x : Nat -> (Nat -> Bool). 1)"))