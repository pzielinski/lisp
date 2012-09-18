(ns scheme.interpreter.environmental.env)

(use 'scheme.interpreter.utils)

(defn the-empty-environment 
  [] 
  '{})

(defn empty-environment? 
  [env] 
  (empty? env))

(defn 
  set-variable-value-in-env 
  [variable value env]
    (assoc env variable value)
)

(defn 
  lookup-variable-value-in-env
  [variable env]
    (get env variable)
)

(defn define-variable! 
  [variable value env]
    (set-variable-value-in-env variable value env)
  )

(defn extend-environment 
  [variables values env] 
  (if (and (empty? variables) (empty? values))
    env
    (if ((and (not (empty? variables)) (not (empty? values))))
      (extend-environment (rest variables) (rest values) (set-variable-value-in-env (first variables) (first values) env))
      (error "Variables/values counts do not match!" (list variables values))
      )
    )
  )

(defn extend-environment-with-map 
  [map env]
  (merge env map)
  )
