(ns user
  (:require [reloaded.repl :refer [system reset stop]]
            [cas.system]))

(reloaded.repl/set-init! #'cas.system/create-system)
