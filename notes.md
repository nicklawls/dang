# Implementation Notes

Following a pretty standard parse-typecheck-interpret flow:

## Parsing

First pass is parsing with an [Instaparse](https://github.com/Engelberg/instaparse) grammar. I still don't really know how ordered choice works... but it works!

Then I used the `insta/transform` function to tidy up the syntax tree a bit: parse boolean and nat literals into their respective clojure types, parse variables into symbols, builtins into keywords, and type declarations into binary trees of keywords.

## Typechecking

I had been itching to try out [meander](https://github.com/noprompt/meander) and this ended up being the perfect opportunity. For instance:

```clojure
[:app
  (m/app check [?in ?out])
  (m/app check ?in)]
?out
```

There's no need to manually compare types for equality, it's all handled implicitly in the names of the pattern variables.

I got a little drunk with this power and put all the logic, including recursion, in the patterns... I regret nothing.

## Evaluation

By far the most involved part... `fix` threw me for a loop.

But in debugging fix, I switched to implementing the core logic as a small-step evaluator, i.e. returning just the application of a single reduction rule to the input expression. This is even less efficient than a big-step tree walking interpreter, but it was quite helpful for me to interactively evaluate expressions one step at a time.
