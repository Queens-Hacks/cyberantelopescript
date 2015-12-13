(defproject cyberantelopescript "0.0.1"
  :plugins [[lein-cljsbuild "1.1.1"]]
  :cljsbuild {
              :builds [{
                        :source-paths ["src-cljs"]
                        :compiler {
                                   :output-to "war/javascript/main.js"
                                   :optimizations :whitespace
                                   :pretty-print true
                                   }
                        }]
              })

