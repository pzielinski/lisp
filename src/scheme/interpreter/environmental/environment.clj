(ns scheme.interpreter.environmental.environment)

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
)

(defn 
  lookup-variable-frame 
  ([variable env]
    (if (not (empty? env)) 
      (let [frames (for [frame env :when (lookup-variable-value-in-frame variable frame)] frame)]
        (if (not (empty? frames))
          (first frames))
        )))
)

(defn 
  lookup-variable-values 
  ([variable env]
    (if (empty? env) 
      '()
      (filter #(not (nil? %))
        (for [frame env]
          (lookup-variable-value-in-frame variable frame)))))
)

(defn 
  lookup-variable-value 
  ([variable env]
    (let [values (lookup-variable-values variable env)]
      (if (empty? values) 
        (error "Unbound variable!" variable)
        (first values))))
)

(defn set-binding-value-in-frame! 
  ([variable value frame]
    (dosync (alter (frame :bindings) assoc variable value))
    )     
)

(defn define-variable! 
  ([variable value env]
    (set-binding-value-in-frame! variable value (first-frame env))
    variable
    ) 
  )

(defn set-variable-value! 
  ([variable value env]
    (if (empty-environment? env)
      (error "No frames in environment!" env)
      (let [frame (lookup-variable-frame variable env)
            frame-to-set (if (not (nil? frame)) frame (first-frame env))]
        (set-binding-value-in-frame! variable value frame-to-set))))
  )
