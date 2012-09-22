(ns scheme.interpreter.environmental.s-test)

(use 'clojure.test)
;(use 'clojure.test.junit)

(use 'scheme.interpreter.environmental.env)
(use 'scheme.interpreter.environmental.s)

(deftest primitive-procedure-impl-test
  (is (= first (primitive-procedure-impl (hash-map 'car first) 'car)))
  )

(deftest make-primitive-test 
  (is (= (list 'primitive cons) (make-primitive cons)))
  )

(deftest make-primitives-map-test 
  (is (= '{} (make-primitives-map '{})))
  (is (= (make-primitive first) ((make-primitives-map (hash-map 'car first)) 'car)))
  )

(deftest can-analyze-from-map?-test 
  (is (= true (can-analyze-from-map? global-analyze-map '(quote x))))
  (is (= false (can-analyze-from-map? global-analyze-map '(x))))
  (is (= false (can-analyze-from-map? global-analyze-map '())))
  )

(deftest do-analyze-from-map-test 
  (is (= 'x (get-result-return ((do-analyze-from-map global-analyze-map '(quote x)) nil))))
  )

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
    (is (= 1 (get-result-return (do-eval 'v (extend-environment-with-map '{v 1} env)))))
    ;quoted
    (is (= 'x (get-result-return (do-eval '(quote x) env))))
    ;assignment
    (let [e1 (extend-environment-with-map '{} env)]
      (let [e2 (get-result-env (do-eval '(set! x 2) e1))]
        (is (= 2 (lookup-variable-value-in-env 'x e2)))
        (let [e3 (get-result-env (do-eval '(set! y (+ 1 2)) e2))]
          (is (= 3 (lookup-variable-value-in-env 'y e3)))
          (let [e4 (get-result-env (do-eval '(set! x (+ x y)) e3))]
            (is (= 5 (lookup-variable-value-in-env 'x e4)))
            ))))
    ;definition
    (let [e1 (extend-environment-with-map '{} env)]
      (let [e2 (get-result-env (do-eval '(define x 2) e1))]
        (is (= 2 (lookup-variable-value-in-env 'x e2)))
        (let [e3 (get-result-env (do-eval '(define y (+ 1 2)) e2))]
          (is (= 3 (lookup-variable-value-in-env 'y e3)))
          (let [e4 (get-result-env (do-eval '(define x (+ x y)) e3))]
            (is (= 5 (lookup-variable-value-in-env 'x e4)))
            (let [e5 (get-result-env (do-eval '(define doubler (lambda (x) (+ x x))) e4))]
              (is (= 6 (get-result-return (do-eval '(doubler 3) e5))))
              )))))
    ;definition of function
    (let [e1 (extend-environment-with-map '{} env)]
      (let [e2 (get-result-env (do-eval '(define (doubler x) (+ x x)) e1))]
        (is (not (nil? (lookup-variable-value-in-env 'doubler e2))))
        (let [e3 (get-result-env (do-eval '(define y (doubler 3)) e2))]
          (is (= 6 (lookup-variable-value-in-env 'y e3)))
          )))
    ;if
    (is (= 1 (get-result-return (do-eval '(if true 1 2) env))))
    (is (= 2 (get-result-return (do-eval '(if false 1 2) env))))
    (is (= 'a (get-result-return (do-eval '(if (> 2 1) 'a 'b) env))))
    (is (= 3 (get-result-return (do-eval '(if (> 2 1) (+ 1 2) 'b) env))))
    (is (= 5 (get-result-return (do-eval '(if (< 2 1) (+ 1 2) (- 7 2)) env))))
    ;lambda
    (is (= 5 (get-result-return (do-eval '((lambda (a b) (+ a b)) 2 3) env))))
    ;begin
    (let [e1 (get-result-env (do-eval '(begin (define x 2) (set! y (+ x 1))) env))]
      (is (= 2 (lookup-variable-value-in-env 'x e1)))
      (is (= 3 (lookup-variable-value-in-env 'y e1)))
      )
    ;cond
    (is (= 1 (get-result-return (do-eval '(cond (true 1) (true 2) (else 3)) env))))
    (is (= 2 (get-result-return (do-eval '(cond (false 1) (true 2) (else 3)) env))))
    (is (= 3 (get-result-return (do-eval '(cond (false 1) (false 2) (else 3)) env))))
    (is (= 3 (get-result-return (do-eval '(cond (false 1) (false 2) (true 3) (else 4)) env))))
    (is (= 2 (get-result-return (do-eval '(cond ((= 1 (+ 1 1)) 1) ((= 2 (+ 1 1)) 2) (else 3)) env))))
    (is (= 3 (get-result-return (do-eval '(cond (false 1) (false 2) (else (+ 1 2))) env))))
    ;application
    (is (= 2 (get-result-return (do-eval '(* 1 2) env))))
    (is (= 3 (get-result-return (do-eval '((lambda (a b) (+ a b)) 1 2) env))))
    (is (= 5 (get-result-return (do-eval '((lambda (a b) (+ a b)) (+ 1 2) 2) env))))
    (is (= 10 (get-result-return (do-eval '((lambda (a b) (+ a b)) (+ 1 2) ((lambda (a b) (+ a b)) 3 4)) env))))
    )
  )

;(define fib (lambda (n) (cond ((= n 0) 0) ((= n 1) 1) (else (+ (fib (- n 1)) (fib (- n 2)))))))
;(define mulx (lambda (n x) (if (= n 1) x (mulx (- n 1) (* n x)))))

(run-tests)
;(run-all-tests) 
;(with-junit-output (run-tests))
;(run-tests)
;C:\Users\Piotr\workspace\lisp\classes>java -classpath .\..\clojure.jar;C:\Users\Piotr\workspace\lisp\classes clojure.main -i "scheme\interpreter\environmental\s.clj" -e "(use 'clojure.test) (run-all-tests)"
