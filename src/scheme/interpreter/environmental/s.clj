(ns scheme.interpreter.environmental.s)

(use 'clojure.test)
;(use 'clojure.test.junit)

(use 'scheme.interpreter.utils)
(use 'scheme.interpreter.syntax)
(use 'scheme.interpreter.environmental.environment)

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

(defn primitive-procedure-impl
  ([primitive-procedure-impl-map, k]
    (primitive-procedure-impl-map k))
  {
   :test
   (fn []
       (is (= first (primitive-procedure-impl (hash-map 'car first) 'car)))
     )
   }
)

(defn make-begin 
  [the-sequence] 
  (cons 'begin the-sequence))

(defn make-if 
  [predicate consequent alternative]
  (list 'if predicate consequent alternative))

(defn make-primitive 
  ([implementation] 
    (list 'primitive implementation))
  {
   :test
   (fn []
       (is (= (list 'primitive cons) (make-primitive cons)))
     )
   }
)

(defn make-primitives-map 
  ([primitive-procedure-impl-map]
    (map-the-map identity #(make-primitive %) primitive-procedure-impl-map)
  )
  {
   :test
   (fn []
       (is (= '{} (make-primitives-map '{})))
       (is (= (make-primitive first) ((make-primitives-map (hash-map 'car first)) 'car)))
     )
   }
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
  (fn [env] exp)
  )

(defn analyze-variable 
  [exp] 
  (fn [env] (lookup-variable-value exp env))
  )

(defn analyze-quotation 
  [exp]
  (let [value (text-of-quotation exp)]
    (fn [env] value))
  )

(defn analyze-assignment 
  [exp] 
  (let [variable (assignment-variable exp)
        value-proc (analyze (assignment-value exp))
        ]
    (fn [env] (set-variable-value! variable (value-proc env) env) 'ok)
    )
  )

(defn analyze-cond 
  [exp]
  (let [condifexp-proc (analyze (cond->if exp))] 
    (fn [env] (condifexp-proc env))
    )
  )

(defn analyze-definition-of-function 
  [exp] 
  (let [variable (definition-variable exp)
        function-name (first variable)
        params (rest variable)
        body-proc (analyze (definition-value exp)) ;single expression only !!!
        ]

    (fn [env] (define-variable! function-name (make-procedure params body-proc env) env) 'ok)
    )
  )

(defn analyze-definition-of-variable 
  [exp] 
  (let [variable (definition-variable exp)
        value-proc (analyze (definition-value exp))
        ]
    (fn [env] (define-variable! variable (value-proc env) env) 'ok)
    )
  )

(defn analyze-definition 
  [exp] 
  (let [variable (definition-variable exp)]
    (if (variable? variable)
      (analyze-definition-of-variable exp)
      (analyze-definition-of-function exp)))
  )

(defn analyze-if 
  [exp]
  (let [predicate-proc (analyze (if-predicate exp))
        consequent-proc (analyze (if-consequent exp))
        alternative-proc (analyze (if-alternative exp))
        ]
    (fn [env]
      (if (true? (predicate-proc env))
        (consequent-proc env)
        (alternative-proc env))
      )
    )
  )

(defn analyze-lambda 
  [exp] 
  (let [params (lambda-parameters exp)
        body-proc (analyze (lambda-body exp)) ;single expression only !!!
        ]
    (fn [env] (make-procedure params body-proc env))
    )
  )

(defn analyze-sequence 
  ([exps]
    (cond
      (empty? exps) (fn [env] )
      (= (count exps) 1) (fn [env] ((analyze (first exps)) env))
      :else
      (reduce 
        (fn [exp1 exp2]
          (let [exp1-proc (analyze exp1)
                exp2-proc (analyze exp2)
                ]        
            (fn [env] (exp1-proc env) (exp2-proc env))
            ))
        exps
        )  
      ;(map-nth #(do-eval % env) exps)
      ))
  {
   :test
   (fn []
       ;(is (= (fn [env] ) (analyze-sequence '())))
     )
   }
  )

(defn analyze-begin
  [exp]
  (analyze-sequence (begin-actions exp)) 
)

(defn execute-application [procedure arguments] 
  (cond 
    (primitive-procedure? procedure) 
       (apply-primitive-procedure procedure arguments)
    (compound-procedure? procedure) 
       (let [params (procedure-parameters procedure)
             proc-env (procedure-environment procedure)
             env (extend-environment params arguments proc-env)
             body (procedure-body procedure) 
             ] 
         (body env)
         )
    :else 
       (error "Unknown procedure type -- APPLY" procedure)))

(defn analyze-application 
  [exp] 
  (let [operator-proc (analyze (operator exp))
        value-procs (map #(analyze %) (operands exp))
        ]
    (fn [env] 
      (let [operator (operator-proc env)
            args (map #(% env) value-procs)
            ]
        (execute-application operator args)))
    )
  )

(defn setup-environment
  ([primitive-procedure-impl-map]
  (let [initial-env (extend-environment-with-frame 
                      (make-frame-from-map 
                        (make-primitives-map 
                          primitive-procedure-impl-map)) 
                      (the-empty-environment))]
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
  {
   :test
   (fn []
       (is (= (make-primitive first) (lookup-variable-value 'car (setup-environment (hash-map 'car first)))))
       (is (= true (lookup-variable-value 'true (setup-environment (hash-map 'car first)))))
       (is (= false (lookup-variable-value 'false (setup-environment (hash-map 'car first)))))
     )
   }
)

(def global-analyze-map
  {
   'quote analyze-quotation
   'set! analyze-assignment
   'define analyze-definition
   'cond analyze-cond
   'if analyze-if
   'lambda analyze-lambda
   'begin analyze-begin
   }
)

(defn can-analyze-from-map? 
  ([the-map exp]
    (if (empty? exp)
      false
      (not (nil? (the-map (first exp))))))
  {
   :test
   (fn []
       (is (= true (can-analyze-from-map? global-analyze-map '(quote x))))
       (is (= false (can-analyze-from-map? global-analyze-map '(x))))
       (is (= false (can-analyze-from-map? global-analyze-map '())))
     )
   }
)

(defn do-analyze-from-map 
  ([the-map exp]
  (let [proc (the-map (first exp))]
    (proc exp)))
  {
   :test
   (fn []
       (is (= 
             'x 
             ((do-analyze-from-map global-analyze-map '(quote x)) nil)
             )
         )
     )
   }
)

(defn analyze 
  ([exp]
    (cond 
      (self-evaluating? exp) (analyze-self-evaluating exp)
      (variable? exp) (analyze-variable exp)
      (can-analyze-from-map? global-analyze-map exp) (do-analyze-from-map global-analyze-map exp)
      (application? exp) (analyze-application exp)
      :else (error "Unknown expression type -- EVAL" exp)))
)

(defn do-eval 
  ([exp env]
    (let [proc (analyze exp)]
      (proc env)
      )
    )
  {
   :test
   (fn []
     (let [env (setup-environment global-primitive-procedure-impl-map)]
       ;primitive
       (is (= false (do-eval 'false env)))
       (is (= true (do-eval 'true env)))
       (is (= (make-primitive +) (do-eval '+ env)))
       (is (= (make-primitive first) (do-eval 'car env)))
       ;self-evaluating
       (is (= 1 (do-eval 1 env)))
       (is (= "" (do-eval "" env)))
       (is (= "1" (do-eval "1" env)))
       ;variable
       (is (thrown? Throwable (do-eval 'v env)) "Variable is not bound!")
       (is (= 1 (do-eval 'v (extend-environment-with-frame (make-frame-from-map '{v 1}) env))))
       ;quoted
       (is (= 'x (do-eval '(quote x) env)))
       ;assignment
       (let [f1 (make-frame-from-map '{})
             e1 (extend-environment-with-frame f1 env)]
         (do-eval '(set! x 2) e1)
         (is (= 2 (lookup-variable-value 'x e1)))
         (do-eval '(set! y (+ 1 2)) e1)
         (is (= 3 (lookup-variable-value 'y e1)))
         (do-eval '(set! x (+ x y)) e1)
         (is (= 5 (lookup-variable-value 'x e1)))
       )
       ;definition
       (let [f1 (make-frame-from-map '{})
             e1 (extend-environment-with-frame f1 env)]
         (do-eval '(define x 2) e1)
         (is (= 2 (lookup-variable-value 'x e1)))
         (do-eval '(define y (+ 1 2)) e1)
         (is (= 3 (lookup-variable-value 'y e1)))
         (do-eval '(define x (+ x y)) e1)
         (is (= 5 (lookup-variable-value 'x e1)))
         (do-eval '(define doubler (lambda (x) (+ x x))) e1)
         (is (= 6 (do-eval '(doubler 3) e1)))
       )
       ;definition of function
       (let [f1 (make-frame-from-map '{})
             e1 (extend-environment-with-frame f1 env)]
         (do-eval '(define (doubler x) (+ x x)) e1)
         (is (not (nil? (lookup-variable-value 'doubler e1))))
         (do-eval '(define y (doubler 3)) e1)
         (is (= 6 (lookup-variable-value 'y e1)))
       )
       ;if
       (is (= 1 (do-eval '(if true 1 2) env)))
       (is (= 2 (do-eval '(if false 1 2) env)))
       (is (= 'a (do-eval '(if (> 2 1) 'a 'b) env)))
       (is (= 3 (do-eval '(if (> 2 1) (+ 1 2) 'b) env)))
       (is (= 5 (do-eval '(if (< 2 1) (+ 1 2) (- 7 2)) env)))
       ;lambda
       (is (= 5 (do-eval '((lambda (a b) (+ a b)) 2 3) env)))
       ;begin
       (let [f1 (make-frame-from-map '{})
             e1 (extend-environment-with-frame f1 env)]
         (do-eval '(begin (define x 2) (set! y (+ x 1))) e1)
         (is (= 2 (lookup-variable-value 'x e1)))
         (is (= 3 (lookup-variable-value 'y e1)))
         )
       ;cond
       (is (= 1 (do-eval '(cond (true 1) (true 2) (else 3)) env)))
       (is (= 2 (do-eval '(cond (false 1) (true 2) (else 3)) env)))
       (is (= 3 (do-eval '(cond (false 1) (false 2) (else 3)) env)))
       (is (= 3 (do-eval '(cond (false 1) (false 2) (true 3) (else 4)) env)))
       (is (= 2 (do-eval '(cond ((= 1 (+ 1 1)) 1) ((= 2 (+ 1 1)) 2) (else 3)) env)))
       (is (= 3 (do-eval '(cond (false 1) (false 2) (else (+ 1 2))) env)))
       ;application
       (is (= 2 (do-eval '(* 1 2) env)))
       (is (= 3 (do-eval '((lambda (a b) (+ a b)) 1 2) env)))
       (is (= 5 (do-eval '((lambda (a b) (+ a b)) (+ 1 2) 2) env)))
       (is (= 10 (do-eval '((lambda (a b) (+ a b)) (+ 1 2) ((lambda (a b) (+ a b)) 3 4)) env)))
     ))
   }
)
;(do-eval '((lambda (a b) (+ a b)) 1 2) global-environment)
;(define fib (lambda (n) (cond ((= n 0) 0) ((= n 1) 1) (else (+ (fib (- n 1)) (fib (- n 2)))))))

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

(def input-prompt ";;; input:")

(def output-prompt ";;; value:")

(defn driver-loop
  ([env]
    (println input-prompt)
    (let [input (read)]
      (let [output (do-eval input env)]
        (println output-prompt)
        (user-print output)
        )
      )
    )
  )

(def global-environment 
  (setup-environment global-primitive-procedure-impl-map))

(defn s 
  []
  (driver-loop global-environment)
  (s))

;(run-all-tests) 
;(with-junit-output (run-tests))
;(run-tests)
;C:\Users\Piotr\workspace\lisp\classes>java -classpath .\..\clojure.jar;C:\Users\Piotr\workspace\lisp\classes clojure.main -i "scheme\interpreter\environmental\s.clj" -e "(use 'clojure.test) (run-all-tests)"
