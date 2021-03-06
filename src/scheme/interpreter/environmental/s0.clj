(ns scheme.interpreter.environmental.s0)

(use 'clojure.test)
;(use 'clojure.test.junit)

(use 'scheme.interpreter.utils)
(use 'scheme.interpreter.syntax)
(use 'scheme.interpreter.environmental.env)

;clojure specific
(def apply-in-underlying-interpreter 
  apply)

(def global-primitive-procedure-impl-map
  {
   '* *
   '+ +
   '- -
   '> >
   '< <
   '= =
   '>= >=
   '<= <=
   '== ==
   'car first
   'cdr rest
   'cons cons
   'null? nil?
   }
)

(declare analyze)
(declare do-eval)

(defn result?
  [obj]
    (map? obj)
  )

(defn make-result
  [return env]
    {:return return :env env}
  )

(defn get-result-return
  [eval-result]
    (:return eval-result)
  )

(defn get-result-env
  [eval-result]
    (:env eval-result)
  )

(defn delay-it
  [proc env]
  (list 'thunk proc env)
  )

(defn thunk?
  [obj]
  (tagged-list? obj 'thunk)
  )

(defn thunk-proc
  [obj]
  (nth obj 1)
  )

(defn thunk-env
  [obj]
  (nth obj 2)
  )

(defn force-it
  [objx env]
  (if (result? objx)
    (let [obj (get-result-return objx)]
      (if (not (thunk? obj))
        objx;was from result - do not eval - return result!
        (let [new-proc (thunk-proc obj)
              new-env (thunk-env obj)
              new-obj (new-proc new-env)]
          (force-it new-obj env))))
    (let [obj objx]
      (if (not (thunk? obj))
        (force-it (obj env) env);was not result - must be proc - so eval
        (let [new-proc (thunk-proc obj)
              new-env (thunk-env obj)
              new-obj (new-proc new-env)]
          (force-it new-obj env)
        ))));TODO: memoize!
  )

(defn primitive-procedure-impl
  [primitive-procedure-impl-map, k]
  (primitive-procedure-impl-map k)
  )

(defn make-begin 
  [the-sequence] 
  (cons 'begin the-sequence)
  )

(defn make-if 
  [predicate consequent alternative]
  (list 'if predicate consequent alternative)
  )

(defn make-primitive 
  [implementation] 
  (list 'primitive implementation)
  )

(defn make-primitives-map 
  [primitive-procedure-impl-map]
  (map-the-map identity (fn [x] (make-primitive x)) primitive-procedure-impl-map)
  )

(defn make-procedure 
  [parameters body env] 
  (list 'procedure parameters body env))

(defn apply-primitive-procedure 
  [procedure arguments]
  (apply-in-underlying-interpreter (primitive-implementation procedure) arguments))

(defn assignment-variable 
  [exp] 
  (nth exp 1))

(defn assignment-value 
  [exp] 
  (nth exp 2))

(defn begin-actions 
  [exp] 
  (rest exp))

(defn cond-clauses 
  [exp] 
  (rest exp))

(defn cond-clause-actions 
  [clause] 
  (rest clause))

(defn cond-clause-predicate 
  [clause] 
  (first clause))

(defn cond-else-clause? 
  [clause] 
  (= (cond-clause-predicate clause) 'else))

(defn sequence->exp 
  [the-sequence]
  (cond 
    (empty? the-sequence) the-sequence
    (= (count the-sequence) 1) (first the-sequence)
    :else (make-begin the-sequence)))

(defn expand-clauses [clauses]
  (if (empty? clauses)
    'false
    (if (cond-else-clause? (first clauses))
      (if (empty? (rest clauses))
        (sequence->exp (cond-clause-actions (first clauses)))
        (error "ELSE clause isn't last -- COND->IF" clauses))
      (make-if 
        (cond-clause-predicate (first clauses)) 
        (sequence->exp (cond-clause-actions (first clauses)))
        (expand-clauses (rest clauses))))))

(defn cond->if 
  [exp] 
  (expand-clauses (cond-clauses exp)))

(defn definition-variable 
  [exp] 
  (nth exp 1))

(defn definition-value 
  [exp] 
  (nth exp 2))

(defn text-of-quotation 
  [exp] 
  (nth exp 1))

(defn if-alternative 
  [exp] 
  (if (= (count exp) 4) (nth exp 3) 'false))

(defn if-consequent 
  [exp] 
  (nth exp 2))

(defn if-predicate 
  [exp] 
  (nth exp 1))

(defn lambda-body 
  [exp] 
  (nth exp 2))

(defn lambda-parameters 
  [exp] 
  (nth exp 1))

(defn operands 
  [exp] 
  (rest exp))

(defn operator 
  [exp] 
  (first exp))

(defn procedure-body 
  [exp] 
  (nth exp 2))

(defn procedure-environment 
  [exp] 
  (nth exp 3))

(defn procedure-parameters 
  [exp] 
  (nth exp 1))

(defn analyze-self-evaluating 
  [exp] 
  (fn [env] 
    (make-result exp env)
    )
  )

(defn force-delayed-val 
  [value-delayed-or-not env] 
  (if (tagged-list? value-delayed-or-not 'delayed-val)
    (let [value-proc (tagged-list-content-1 value-delayed-or-not)
          value-result (value-proc env)]
          value-result)
    (make-result value-delayed-or-not env))
  )

(defn analyze-variable 
  [exp] 
  (fn [env] 
    (let [value-delayed-or-not (lookup-variable-value-in-env exp env)
          value (force-delayed-val value-delayed-or-not env)]
      value))
  )

(defn analyze-quotation 
  [exp]
  (fn [env]
    (make-result (text-of-quotation exp) env)
    )
  )

(defn analyze-assignment 
  [exp] 
  (let [variable (assignment-variable exp)
        value-proc (analyze (assignment-value exp))]
    (fn [env]
      (make-result 
        'ok 
        (set-variable-value-in-env variable (get-result-return (value-proc env)) env));use new env
      )
    )
  )

(defn analyze-cond 
  [exp]
  (let [condifexp-proc (analyze (cond->if exp))] 
    (fn [env]
      (let [cond-result (condifexp-proc env)
            cond-return-val (get-result-return cond-result)]
        (make-result cond-return-val env);use original env
        )
      )
    )
  )

(defn analyze-definition-of-function 
  [exp] 
  (let [variable (definition-variable exp)
        function-name (first variable)
        params (rest variable)
        body-proc (analyze (definition-value exp))];single expression only, use begin!!!
    (fn [env] 
      (make-result 
        'ok
        (set-variable-value-in-env function-name (make-procedure params body-proc env) env ))));new env
  )

(defn analyze-definition-of-variable 
  [exp] 
  (let [variable (definition-variable exp)
        value-proc (analyze (definition-value exp))]
    (fn [env]
      (make-result 
        'ok
        (let [env1 (set-variable-value-in-env variable (list 'delayed-val value-proc) env)
              value-delayed (lookup-variable-value-in-env variable env1)
              value-result (force-delayed-val value-delayed env1)
              value (get-result-return value-result)
              env2 (set-variable-value-in-env variable value env1)]
          env2);pass new env
        )))
  )

(defn analyze-definition 
  [exp] 
  (let [variable (definition-variable exp)]
    (if (variable? variable)
      (analyze-definition-of-variable exp)
      (analyze-definition-of-function exp))
    )
  )

(defn analyze-lambda 
  [exp] 
  (let [params (lambda-parameters exp)
        body-proc (analyze (lambda-body exp));single expression only, use begin!
        ]
    (fn [env]
      (make-result (make-procedure params body-proc env) env)
      ))
  )

(defn analyze-if 
  [exp]
  (let [predicate-proc (analyze (if-predicate exp))
        consequent-proc (analyze (if-consequent exp))
        alternative-proc (analyze (if-alternative exp))]
    (fn [env]
      (if (true? (get-result-return (force-it predicate-proc env)))
        (consequent-proc env)
        (alternative-proc env))
      )
    )
  )

(defn analyze-sequence 
  [exps]
  (cond
    (empty? exps) 
    (fn [env] )
    (= (count exps) 1) 
    (let [exp (first exps)
          exp-proc (analyze exp)]
      (fn [env] 
        (let [exp-result (exp-proc env) 
              exp-ret (get-result-return exp-result)
              exp-env (get-result-env exp-result)]
          (make-result exp-ret exp-env)))
      )
    :else
    (reduce 
      (fn [fn1 fn2]
        (fn [env] 
          (let [exp1-result (fn1 env) 
                exp1-ret (get-result-return exp1-result)
                exp1-env (get-result-env exp1-result)
                exp2-result (fn2 exp1-env);use env returned by eval of previous exp
                exp2-ret (get-result-return exp2-result)
                exp2-env (get-result-env exp2-result)]
            (make-result exp2-ret exp2-env))))
      ;create a list of functions fn[env]
      (map 
        (fn [exp] 
          (let [exp-proc (analyze exp)]
            (fn [env] 
              (let [exp-result (exp-proc env) 
                    exp-ret (get-result-return exp-result)
                    exp-env (get-result-env exp-result)]
                (make-result exp-ret exp-env)));pass new env
            )) 
        exps)
      )
    )
  )

(defn analyze-begin
  [exp]
  (analyze-sequence (begin-actions exp)) 
)

(defn analyze-application
  [exp] 
  (let [operator-proc (analyze (operator exp))
        arg-procs (map #(analyze %) (operands exp))]
    (fn [env] 
      (let [procedure (get-result-return (force-it operator-proc env))];force operator!
        (make-result 
          (cond 
            (primitive-procedure? procedure)
            (let [args (map (fn [arg-proc] (get-result-return (force-it arg-proc env))) arg-procs)]
              ;(println 'apply-primitive-procedure procedure args)
              (apply-primitive-procedure procedure args))
            (compound-procedure? procedure) 
            (let [params (procedure-parameters procedure)
                  arg-procs-delayed (map (fn [arg-proc] (delay-it arg-proc env)) arg-procs)
                  proc-env (procedure-environment procedure)
                  new-env (extend-environment params arg-procs-delayed proc-env)
                  body (procedure-body procedure)]
              ;(println 'apply-compound-procedure 'PROC= procedure 'ARGS= arg-procs-delayed 'END )
              (get-result-return (body new-env))
              )
            :else 
            (error "Unknown procedure type -- APPLY" procedure))
          env
          )
        )
      )
    )
  )

(defn setup-environment
  [primitive-procedure-impl-map env]
  (let [primitives-map (make-primitives-map primitive-procedure-impl-map)
        env1 (set-variable-value-in-env 'true true env)
        env2 (set-variable-value-in-env 'false false env1)]
    (extend-environment-with-map primitives-map env2)
    )
  )

(def global-analyze-map
  {
   'quote analyze-quotation
   'cond analyze-cond
   'if analyze-if
   'lambda analyze-lambda
   'begin analyze-begin
   }
)

(defn can-analyze-from-map? 
  [the-map exp]
    (if (empty? exp)
      false
      (not (nil? (the-map (first exp)))))
)

(defn define?
  [exp]
  (if (empty? exp)
    false
    (= 'define (first exp)))
  )

(defn set!?
  [exp]
  (if (empty? exp)
    false
    (= 'set! (first exp)))
  )

(defn do-analyze-from-map 
  [the-map exp]
  (let [proc (the-map (first exp))]
    ;each proc returns fn[env] that returns map
    (proc exp))
)

(defn analyze 
  [exp]
  (cond 
    (self-evaluating? exp) (analyze-self-evaluating exp)
    (variable? exp) (analyze-variable exp)
    (can-analyze-from-map? global-analyze-map exp) (do-analyze-from-map global-analyze-map exp)
    (define? exp) (analyze-definition exp)
    (set!? exp) (analyze-assignment exp)
    (application? exp) (analyze-application exp)
    :else (error "Unknown expression type -- EVAL" exp))
  )

(defn do-eval 
  [exp env]
  (let [proc (analyze exp)]
    (force-it proc env)
    )
  )

(defn user-print 
  [object]
  (if (compound-procedure? object)
    (println 
      (list 
        'compound-procedure
        (procedure-parameters object)
        (procedure-body object)
        '<procedure-env>))
    (println object)
    )
  )

(def input-prompt "in:")

(def output-prompt "=>")

(defn repl
  []
  (loop [env (setup-environment global-primitive-procedure-impl-map (the-empty-environment))]
    (let [input (read)]
      (println input)
      (if (= (str input) "exit")
        (println "exit requested by user")
        (if (nil? input)
          (recur env)
          (let [output-result (do-eval input env)
                output-value (get-result-return output-result)
                output-env (get-result-env output-result)]
            (print output-prompt)
            (user-print output-value)
            (recur output-env)
            )
          )
        )
      )
    )
  )
