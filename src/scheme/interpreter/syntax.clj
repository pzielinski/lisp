(ns scheme.interpreter.syntax)

(use 'clojure.test)
;(use 'clojure.test.junit)

(defn tagged-list? 
  [exp the-tag]
  (if (list? exp)
    (= (first exp) the-tag)
    false))

(defn application? 
  [exp] 
  (list? exp))

(defn assignment? 
  [exp] 
  (tagged-list? exp 'set!))

(defn begin? 
  [exp] 
  (tagged-list? exp 'begin))

(defn compound-procedure? 
  [exp] 
  (tagged-list? exp 'procedure))

(defn cond? 
  [exp] 
  (tagged-list? exp 'cond))

(defn definition? 
  [exp] 
  (tagged-list? exp 'define))

(defn if? 
  [exp] 
  (tagged-list? exp 'if))

(defn lambda? 
  [exp] 
  (tagged-list? exp 'lambda))

(defn primitive-procedure? 
  [exp] 
  (tagged-list? exp 'primitive))

(defn primitive-implementation 
  [exp] 
  (nth exp 1))

(defn quoted? 
  [exp] 
  (tagged-list? exp 'quote))

(defn self-evaluating? 
  [exp]
  (cond (number? exp) true
    (string? exp) true
    :else false))

(defn variable? 
  [exp] 
  (if (symbol? exp)
    true
    (if (true? exp)
      true
      (if (false? exp)
        true
        false
        )
      )
    )
  )
