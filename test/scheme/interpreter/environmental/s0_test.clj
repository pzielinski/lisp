(ns scheme.interpreter.environmental.s0-test)

(use 'clojure.test)
;(use 'clojure.test.junit)

(use 'scheme.interpreter.environmental.env)
(use 'scheme.interpreter.environmental.s0)

(deftest primitive-procedure-impl-test
  (is (= first (primitive-procedure-impl (hash-map 'car first) 'car)))
  )

(deftest make-primitive-test 
  (is (= (list 'primitive cons) (make-primitive cons)))
  )

(deftest can-analyze-from-map?-test 
  (is (= true (can-analyze-from-map? global-analyze-map '(quote x))))
  (is (= false (can-analyze-from-map? global-analyze-map '(x))))
  (is (= false (can-analyze-from-map? global-analyze-map '())))
  )

(deftest execute-procs-test 
  (is (= true (execute-procs (list (fn [result] true)) {})))
  (is (= :a (execute-procs (list (fn [result] :a) (fn [result] result)) {})))
  (is (= :a (execute-procs (list (fn [result] :a) (fn [result] result) (fn [result] result)) {})))
  )

(def factorial
  (fn [n]
    (loop [cnt n acc 1]
       (if (zero? cnt)
            acc
          (recur (dec cnt) (* acc cnt))))))

(deftest do-eval-test 
  (let [env (setup-environment global-primitive-procedure-impl-map (the-empty-environment))]
    ;primitive
    (is (= false (get-result-return (do-eval 'false env))))
    (is (= true (get-result-return (do-eval 'true env))))
    (is (= (make-primitive +) (get-result-return (do-eval '+ env))))
    (is (= (make-primitive first) (get-result-return (do-eval 'car env))))
    ;self-evaluating
    (is (= 1 (get-result-return (do-eval 1 env))))
    (is (= "" (get-result-return (do-eval "" env))))
    (is (= "1" (get-result-return (do-eval "1" env))))
    ;variable
    ;(is (thrown? Throwable (do-eval 'v env)) "Variable is not bound!");!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    (is (= 1 (get-result-return (do-eval 'v (extend-environment-with-map {'v 1} env)))))
    ;quoted
    (is (= 'x (get-result-return (do-eval '(quote x) env))))
    ;application & lambda
    (is (= 2 (get-result-return (do-eval '(* 1 2) env))))
    (is (= 3 (get-result-return (do-eval '((lambda (a b) (+ a b)) 1 2) env))))
    (is (= 5 (get-result-return (do-eval '((lambda (a b) (+ a b)) (+ 1 2) 2) env))))
    (is (= 10 (get-result-return (do-eval '((lambda (a b) (+ a b)) (+ 1 2) ((lambda (a b) (+ a b)) 3 4)) env))))
    ;definition
    (let [e1 (extend-environment-with-map '{} env)]
      (let [e2 (get-result-env (do-eval '(define x 2) e1))]
        (is (= 2 (get-result-return (do-eval 'x e2))))
        (let [e3 (get-result-env (do-eval '(define y (+ 1 x)) e2))]
          (is (= 3 (get-result-return (do-eval 'y e3))))
          (let [e4 (get-result-env (do-eval '(define z (+ x y)) e3))];same here, x causes stack overflow!
            (is (= 5 (get-result-return (do-eval 'z e4))))
            (let [e5 (get-result-env (do-eval '(define doubler (lambda (x) (+ x x))) e4))]
              (is (= 6 (get-result-return (do-eval '(doubler 3) e5))))
              )))))
    ;definition of function via lambda
    (let [e1 (extend-environment-with-map '{} env)]
      (let [e2 (get-result-env (do-eval '(define doubler (lambda (x) (+ x x))) e1))]
        (is (not (nil? (lookup-variable-value-in-env 'doubler e2))))
        (let [e3 (get-result-env (do-eval '(define y (doubler 3)) e2))]
          (is (= 6 (get-result-return (do-eval 'y e3))))
          )))
    ;if
    (is (= 1 (get-result-return (do-eval '(if true 1 2) env))))
    (is (= 2 (get-result-return (do-eval '(if false 1 2) env))))
    (is (= 'a (get-result-return (do-eval '(if (> 2 1) 'a 'b) env))))
    (is (= 3 (get-result-return (do-eval '(if (> 2 1) (+ 1 2) 'b) env))))
    (is (= 5 (get-result-return (do-eval '(if (< 2 1) (+ 1 2) (- 7 2)) env))))
    ;clojure
    (let [e1 (get-result-env (do-eval '(define adder (lambda (a) (lambda (x) (+ x a)))) env))
          e2 (get-result-env (do-eval '(define add2 (adder 2)) e1))]
      (is (= 5 (get-result-return (do-eval '(add2 3) e2))))
      (is (= 7 (get-result-return (do-eval '((adder 3) 4) e1))))
      )
    ;begin
    (let [e1 (get-result-env (do-eval '(begin (define x 2) (define y (+ x 1))) env))]
      (is (= 2 (get-result-return (do-eval 'x e1))))
      (is (= 3 (get-result-return (do-eval 'y e1))))
      )
    ;cond
    (is (= 1 (get-result-return (do-eval '(cond (true 1) (true 2) (else 3)) env))))
    (is (= 2 (get-result-return (do-eval '(cond (false 1) (true 2) (else 3)) env))))
    (is (= 3 (get-result-return (do-eval '(cond (false 1) (false 2) (else 3)) env))))
    (is (= 3 (get-result-return (do-eval '(cond (false 1) (false 2) (true 3) (else 4)) env))))
    (is (= 2 (get-result-return (do-eval '(cond ((= 1 (+ 1 1)) 1) ((= 2 (+ 1 1)) 2) (else 3)) env))))
    (is (= 3 (get-result-return (do-eval '(cond (false 1) (false 2) (else (+ 1 2))) env))))
    ;recursion
    (let [e1 (get-result-env (do-eval '(define fact (lambda (n x) (if (= n 1) x (fact (- n 1) (* n x))))) env))]
      (is (= 1 (get-result-return (do-eval '(fact 1 1) e1))))
      (is (= 2 (get-result-return (do-eval '(fact 2 1) e1))))
      (is (= 6 (get-result-return (do-eval '(fact 3 1) e1))))
      (is (= 24 (get-result-return (do-eval '(fact 4 1) e1))))
      (is (= (factorial 20) (get-result-return (do-eval '(fact 20 1) e1))))
      )
    (let [e1 (get-result-env 
               (do-eval 
                 '(define arithmetic-s (lambda (n sum) (if (= n 0) sum (arithmetic-s (- n 1) (+ n sum))))) 
                 env))]
      (is (= 1 (get-result-return (do-eval '(arithmetic-s 1 0) e1))))
      (is (= 3 (get-result-return (do-eval '(arithmetic-s 2 0) e1))))
      (is (= 6 (get-result-return (do-eval '(arithmetic-s 3 0) e1))))
      (is (= 190 (get-result-return (do-eval '(arithmetic-s 19 0) e1))))
      (let [n 100
            sum (* (/ (+ n 1) 2) n)
            e2 (get-result-env (do-eval (list 'define 'n n) e1))]
        (is (= sum (get-result-return (do-eval '(arithmetic-s n 0) e2)))))
      )
    )
  )

;(define fib (lambda (n) (cond ((= n 0) 0) ((= n 1) 1) (else (+ (fib (- n 1)) (fib (- n 2)))))))
(run-tests)
;(run-all-tests) 
;(with-junit-output (run-tests))
;(run-tests)
;C:\Users\Piotr\workspace\lisp\classes>java -classpath .\..\clojure.jar;C:\Users\Piotr\workspace\lisp\classes clojure.main -i "scheme\interpreter\environmental\s.clj" -e "(use 'clojure.test) (run-all-tests)"
