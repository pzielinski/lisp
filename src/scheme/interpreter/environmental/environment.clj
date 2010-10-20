(ns scheme.interpreter.environmental.environment)

(use 'clojure.test)
;(use 'clojure.test.junit)

(use 'scheme.interpreter.utils)

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
