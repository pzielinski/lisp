;todo: 1) implement define & set! as let 2) iplement let as lambda 
;java -cp clojure.jar clojure.main -i "src/*.clj" -e "(use 'clojure.test) (run-all-tests)"

(ns scheme.interpreter.int-env)

(use 'clojure.test)
;(use 'clojure.test.junit)

;----------------------------------------------------------------------------------------------
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

;----------------------------------------------------------------------------------------------

;util  
(defn make-map-from-pairs
  ([pairs]
    (if (empty? pairs)
      {}
      (reduce 
        #(assoc %1 (first %2) (nth %2 1)) 
        '{}
        pairs
        )))
  {
   :test
   (fn []
       (is (= '{} (make-map-from-pairs '())))
       (is (= '{a 1} (make-map-from-pairs '((a 1)))))
       (is (thrown? Throwable (make-map-from-pairs '((a)))))
       (is (= '{a 1 b 2 c 3} (make-map-from-pairs '((a 1) (b 2) (c 3)))))
     )
   }
  )

(defn map-the-map 
  ([fkey fval the-map]
  (if (empty? the-map)
    {}
    (make-map-from-pairs 
      (map 
        #(list (fkey (key %)) (fval (val %))) 
        the-map))
    ))
  {
   :test
   (fn []
       (is (= '{} (map-the-map nil nil '{})))
       (is (= '{} (map-the-map identity identity '{})))
       (is (= '{a 1 b 2 c 3} (map-the-map identity identity '{a 1 b 2 c 3})))
       (is (= '{a 2 b 4 c 6} (map-the-map identity #(* 2 %) '{a 1 b 2 c 3})))
       (is (= '{ax 1 bx 2 cx 3} (map-the-map #(symbol (str % 'x)) identity '{a 1 b 2 c 3})))
       (is (= '{x 3} (map-the-map (fn [k] 'x) identity '{a 1 b 2 c 3})))
     )
   }
  )

(defn make-map-from-vars-vals 
  ([variables values]
  (if (or (empty? variables) (empty? values))
    {}
    (make-map-from-pairs (map #(list %1 %2) variables values))
    ))
  {
   :test
   (fn []
       (is (= '{} (make-map-from-vars-vals '() '())))
       (is (= '{} (make-map-from-vars-vals '(a) '())))
       (is (= '{} (make-map-from-vars-vals '() '(1))))
       (is (= '{} (make-map-from-vars-vals '(a) nil)))
       (is (= '{} (make-map-from-vars-vals nil '(1))))
       (is (= '{a 1} (make-map-from-vars-vals '(a) '(1 2))))
       (is (= '{a 1} (make-map-from-vars-vals '(a b) '(1))))
       (is (= '{a 1 b 2 c 3} (make-map-from-vars-vals '(a b c) '(1 2 3))))
     )
   }
)

(defn error 
  [msg arg1] 
  (throw ( new Throwable (str msg " " arg1))))

(defn map-nth 
  [f coll] 
  (nth (map f coll) (dec (count coll))))

(defn throw-not-impl 
  [] 
  (throw ( new Throwable "NOT IMPLEMENTED!")))

;------------------------------------------------------------------------------------------------
;syntax

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
;----------------------------------------------------------------------------------------------
;env
(defn make-frame-from-map 
  [the-map]
  {:bindings (ref the-map)})

(defn add-binding-to-frame! 
  [variable value frame]
  (dosync (alter (frame :bindings) assoc variable value)))     

(defn make-frame 
  [variables values]
  (make-frame-from-map (make-map-from-vars-vals variables values)))

(defn the-empty-environment 
  [] 
  '())

(defn empty-environment? 
  [env] 
  (empty? env))

(defn first-frame 
  [env] 
  (first env))

(defn enclosing-environment 
  [env] 
  (rest env))

(defn extend-environment-with-frame 
  [frame env] 
  (cons frame env))

(defn extend-environment 
  [variables values env] 
  (if (= (count variables) (count values))
    (extend-environment-with-frame (make-frame variables values) env)
    (error "Variables/values counts do not match!" (list variables values))))

(defn 
  lookup-variable-value-in-frame 
  ([variable frame]
    (@(frame :bindings) variable))
  {
   :test
   (fn []
     (let[
          emptyFrame (make-frame-from-map '{})
          f1 (make-frame-from-map '{a 1 b 2 c 3})
          ]
       (is (nil? (lookup-variable-value-in-frame 'a emptyFrame)))
       (is (= 1 (lookup-variable-value-in-frame 'a f1)))
       (is (= 2 (lookup-variable-value-in-frame 'b f1)))
       (is (= 3 (lookup-variable-value-in-frame 'c f1)))
       (is (nil? (lookup-variable-value-in-frame 'd f1)))
     ))
   }
)

(defn 
  lookup-variable-frame 
  ([variable env]
    (if (not (empty? env)) 
      (let [frames (for [frame env :when (lookup-variable-value-in-frame variable frame)] frame)]
        (if (not (empty? frames))
          (first frames))
        )))
  {
   :test
   (fn []
     (let[
          emptyFrame (make-frame-from-map '{})
          f1 (make-frame-from-map '{a 1 b 2 c 3})
          f2 (make-frame-from-map '{d 4 e 5 f 6})
          emptyEnv (the-empty-environment)
          e1 (extend-environment-with-frame f1 emptyEnv) 
          e2 (extend-environment-with-frame f2 e1) 
          ]
       (is (= nil (lookup-variable-frame 'a emptyEnv)))
       (is (= f1 (lookup-variable-frame 'a e1)))
       (is (= nil (lookup-variable-frame 'd e1)))
       (is (= f2 (lookup-variable-frame 'd e2)))
       (is (= nil (lookup-variable-frame 'x e2)))
     ))
   }
)

(defn 
  lookup-variable-values 
  ([variable env]
    (if (empty? env) 
      '()
      (filter #(not (nil? %))
        (for [frame env]
          (lookup-variable-value-in-frame variable frame)))))
  {
   :test
   (fn []
     (let[
          emptyFrame (make-frame-from-map '{})
          f1 (make-frame-from-map '{a 1 b 2 c 3})
          f2 (make-frame-from-map '{d 4 e 5 f 6})
          emptyEnv (the-empty-environment)
          e1 (extend-environment-with-frame f1 emptyEnv) 
          e2 (extend-environment-with-frame f2 e1) 
          ]
       (is (= '() (lookup-variable-values 'a emptyEnv)))
       (is (= '(1) (lookup-variable-values 'a e1)))
       (is (= '() (lookup-variable-values 'd e1)))
       (is (= '(4) (lookup-variable-values 'd e2)))
       (is (= '() (lookup-variable-values 'x e2)))
     ))
   }
)

(defn 
  lookup-variable-value 
  ([variable env]
    (let [values (lookup-variable-values variable env)]
      (if (empty? values) 
        (error "Unbound variable!" variable)
        (first values))))
  {
   :test
   (fn []
     (let[
          emptyFrame (make-frame-from-map '{})
          f1 (make-frame-from-map '{a 1 b 2 c 3})
          f2 (make-frame-from-map '{d 4 e 5 f 6})
          emptyEnv (the-empty-environment)
          e1 (extend-environment-with-frame f1 emptyEnv) 
          ]
       (is (thrown? java.lang.Throwable (lookup-variable-value 'a emptyEnv)))
       (is (thrown? java.lang.Throwable (lookup-variable-value 'x e1)))
       (is (= 1 (lookup-variable-value 'a e1)))
     ))
   }
)

(defn set-binding-value-in-frame! 
  ([variable value frame]
    (dosync (alter (frame :bindings) assoc variable value))
    )     
  {
   :test
   (fn []
     (let[
          emptyFrame (make-frame-from-map '{})
          f1 (make-frame-from-map '{a 1 b 2 c 3})
          ]
       (set-binding-value-in-frame! 'a 1 emptyFrame)
       (is (= 1 (lookup-variable-value-in-frame 'a emptyFrame)))
       (set-binding-value-in-frame! 'a 2 emptyFrame)
       (is (= 2 (lookup-variable-value-in-frame 'a emptyFrame)))
       (set-binding-value-in-frame! 'a 3 f1)
       (is (= 3 (lookup-variable-value-in-frame 'a f1)))
     ))
   }
)

(defn define-variable! 
  ([variable value env]
    (set-binding-value-in-frame! variable value (first-frame env))
    variable
    ) 
  {
   :test
   (fn []
     (do
       ;should throw in empty env
       (is (thrown? Throwable (define-variable! 'a 1 (the-empty-environment))))
       ;should add new binding to first frame if no binding
       (let[
            emptyFrame (make-frame-from-map '{})
            e0 (extend-environment-with-frame emptyFrame (the-empty-environment))
          ]
         (define-variable! 'a 1 e0)
         (is (= 1 (lookup-variable-value 'a e0)) "Failed to set variable value in environment!")
         (is (= 1 (lookup-variable-value-in-frame 'a emptyFrame)) "Failed to add new binding in empty frame!")
         )
       ;should change binding value in the first frame only if that binding is defined
       (let[
            f1 (make-frame-from-map '{a 1 b 2 c 3})
            e1 (extend-environment-with-frame f1 (the-empty-environment))
            f2 (make-frame-from-map '{a 10 b 20 c 30})
            e2 (extend-environment-with-frame f2 e1)
          ]
         (define-variable! 'a 100 e2)
         (is (= 100 (lookup-variable-value 'a e2)))
         (is (= 1 (lookup-variable-value-in-frame 'a f1)))
         (is (= 100 (lookup-variable-value-in-frame 'a f2)))
         )
       ;should add new binding to first frame only if no binding in first frame, and many frames
       (let[
            f1 (make-frame-from-map '{a 1 b 2 c 3})
            e1 (extend-environment-with-frame f1 (the-empty-environment))
            f2 (make-frame-from-map '{});empty
            e2 (extend-environment-with-frame f2 e1)
          ]
         (define-variable! 'a 100 e2)
         (is (= 100 (lookup-variable-value 'a e2)))
         (is (= 1 (lookup-variable-value-in-frame 'a f1)))
         (is (= 100 (lookup-variable-value-in-frame 'a f2)))
         )
       )
     )
   }
  )

(defn set-variable-value! 
  ([variable value env]
    (if (empty-environment? env)
      (error "No frames in environment!" env)
      (let [frame (lookup-variable-frame variable env)
            frame-to-set (if (not (nil? frame)) frame (first-frame env))]
        (set-binding-value-in-frame! variable value frame-to-set))))
  {
   :test
   (fn []
     (do
       ;should throw in empty env
       (is (thrown? Throwable (set-variable-value! 'a 1 (the-empty-environment))))
       ;should add new binding to first frame if no binding
       (let[
            emptyFrame (make-frame-from-map '{})
            e0 (extend-environment-with-frame emptyFrame (the-empty-environment))
          ]
         (set-variable-value! 'a 1 e0)
         (is (= 1 (lookup-variable-value 'a e0)) "Failed to set variable value in environment!")
         (is (= 1 (lookup-variable-value-in-frame 'a emptyFrame)) "Failed to add new binding in empty frame!")
         )
       ;should change binding value in the same frame that binding is defined
       (let[
            emptyFrame (make-frame-from-map '{})
            e0 (extend-environment-with-frame emptyFrame (the-empty-environment))
            f1 (make-frame-from-map '{a 1 b 2 c 3})
            e1 (extend-environment-with-frame f1 e0)
          ]
         (set-variable-value! 'a 2 e1)
         (is (= 2 (lookup-variable-value 'a e1)))
         (is (nil? (lookup-variable-value-in-frame 'a emptyFrame)))
         (is (= 2 (lookup-variable-value-in-frame 'a f1)))
         )
       ;should add new binding to first frame if no binding, and many frames
       (let[
            emptyFrame (make-frame-from-map '{})
            e0 (extend-environment-with-frame emptyFrame (the-empty-environment))
            f1 (make-frame-from-map '{a 1 b 2 c 3})
            e1 (extend-environment-with-frame f1 e0)
          ]
         (set-variable-value! 'x 7 e1)
         (is (= 7 (lookup-variable-value 'x e1)))
         (is (nil? (lookup-variable-value-in-frame 'x emptyFrame)))
         (is (= 7 (lookup-variable-value-in-frame 'x f1)))
         )
       )
     )
   }
  )

;----------------------------------------------------------------------------------------------
;implementation

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
  [exp env] 
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

(defn list-of-values 
  [operands env]
  (map #(do-eval % env) operands))

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

(defn eval-assignment 
  [exp env]
  (set-variable-value! 
    (assignment-variable exp) 
    (do-eval (assignment-value exp) env) 
    env)
  )

(defn eval-cond 
  [exp env] 
  (do-eval (cond->if exp) env) ;eval
)

(defn eval-definition 
  [exp env] 
  (define-variable! (definition-variable exp) (do-eval (definition-value exp) env) env))

(defn eval-if 
  [exp env] 
  (if (true? (do-eval (if-predicate exp) env))
    (do-eval (if-consequent exp) env)
    (do-eval (if-alternative exp) env)))

(defn eval-lambda 
  [exp env] 
  (make-procedure (lambda-parameters exp) (lambda-body exp) env))

(defn eval-sequence 
  [exp env] 
  (map-nth #(do-eval % env) (begin-actions exp)))

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

(defn do-apply [procedure arguments] 
  (cond 
    (primitive-procedure? procedure) (apply-primitive-procedure procedure arguments)
    (compound-procedure? procedure) 
    (do-eval ;eval 
      (procedure-body procedure)
      (extend-environment
        (procedure-parameters procedure)
        arguments
        (procedure-environment procedure)))
    :else (error "Unknown procedure type -- APPLY" procedure)))

(def global-eval-map
  {
   'quote text-of-quotation
   'set! eval-assignment
   'define eval-definition
   'cond eval-cond
   'if eval-if
   'lambda eval-lambda
   'begin eval-sequence
   }
)

(defn can-eval-from-map? 
  ([eval-map exp]
    (if (empty? exp)
      false
      (not (nil? (eval-map (first exp))))))
  {
   :test
   (fn []
       (is (= true (can-eval-from-map? global-eval-map '(quote x))))
       (is (= false (can-eval-from-map? global-eval-map '(x))))
       (is (= false (can-eval-from-map? global-eval-map '())))
     )
   }
)

(defn do-eval-from-map 
  ([eval-map exp env]
  (let [proc (eval-map (first exp))]
    (proc exp env)))
  {
   :test
   (fn []
       (is (= 'x (do-eval-from-map global-eval-map '(quote x) nil)))
     )
   }
)

(defn do-eval 
  ([exp env]
    (cond 
      (self-evaluating? exp) exp
      (variable? exp) (lookup-variable-value exp env)
      (can-eval-from-map? global-eval-map exp) (do-eval-from-map global-eval-map exp env)
      (application? exp) (do-apply ;apply
                           (do-eval (operator exp) env);eval
                           (list-of-values (operands exp) env))
      :else (error "Unknown expression type -- EVAL" exp)))
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
       )
       ;if
       (is (= 1 (do-eval '(if true 1 2) env)))
       (is (= 2 (do-eval '(if false 1 2) env)))
       (is (= 'a (do-eval '(if (> 2 1) 'a 'b) env)))
       (is (= 3 (do-eval '(if (> 2 1) (+ 1 2) 'b) env)))
       (is (= 5 (do-eval '(if (< 2 1) (+ 1 2) (- 7 2)) env)))
       ;lambda
       (is (= (make-procedure '(a b) '(+ a b) env) (do-eval '(lambda (a b) (+ a b)) env)))
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
