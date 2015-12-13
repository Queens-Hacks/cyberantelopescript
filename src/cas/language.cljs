(ns cas.language)

;;
;; Defines the language that Robot programs are written in
;;

;; Tokenizes a splitted part of source, ie "124" or "KEYWORD"
(defn tokenize-source-thing [obj]
  (cond
    (re-matches #"[0-9]+" obj) ;; literal int
    (js/parseInt obj)

    ;; keywords
    (= 0 (compare obj "FOR"))
    :for

    (= 0 (compare obj "WHILE"))
    :while

    (= 0 (compare obj "INT"))
    :int

    (= 0 (compare obj "SUB"))
    :sub

    (= 0 (compare obj "{"))
    :open-curly

    (= 0 (compare obj "}"))
    :close-curly

    ;; otherwise, it's a user defined string
    true
    obj

    )
  )

(defn- tokenize [text]
  (map tokenize-source-thing (split text #" ")))

(defn- parse-program [text])

(defn- execuate-ast [ast state])

(defn run [text state]
  (let [ast (parse-program text)]
    (execuate-ast ast state)
    )
  )
