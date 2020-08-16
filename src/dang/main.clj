(ns dang.main
  (:require [clojure.string]
            [clojure.repl]))

(defn println-error [& more]
  (.println *err* (clojure.string/join " " more)))

(comment (println-error "hi" "mom"))

(defn -main []
  (println-error (let [fobble (read-line)]
                   (clojure.string/capitalize fobble)))
  (System/exit 2))


(comment (def arr [1 2 3])
         arr)

(comment (clojure.string/join "," [1 2 3]))

