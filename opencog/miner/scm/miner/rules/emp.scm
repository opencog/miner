;; Rule to calculate the empirical truth value of a pattern.
;;
;; Semi-formally
;;
;; Evaluation (stv 1 1)
;;   Predicate "minsup"
;;   List
;;     <patterm>
;;     <db>
;;     <ms>
;; |-
;; Evaluation <TV>
;;   Predicate "emp"
;;   List
;;     <pattern>
;;     <db>
;;
;; Where TV is calculated using an optimized form of direct
;; calculation. Optimized because some patterns' satisfying sets are
;; too large to have their empirical probability accuratly calculated.

(load "miner-rule-utils.scm")

;; Generate the empirical truth value of a pattern, given the
;; following argument
;;
;; - db-ratio: parameter to control how much downsampling is used to
;;             estimate the empirical probability. The surprisingness
;;             code may automatically downsample the dataset to a
;;             certain amount to reduce the computational burden of
;;             calculating the empirical probability, while preserving
;;             accuracy. However sometimes the user may want to tune
;;             that, either by increasing downsampling so save
;;             computational resources, or decreasing downsampling to
;;             improve accuracy. This parameter indicates the
;;             proportion of the dataset to consider when comparing
;;             the estimate of the pattern count to the dataset
;;             size. The default is 1.0, the rational is that if the
;;             computer has enough RAM to hold a dataset of size s,
;;             then it likely has enough RAM to hold s instances of a
;;             pattern applied over that dataset, so if the estimate
;;             count of instances is below s no subsampling is taking
;;             place, otherwise subsampling is taking place as to not
;;             exceed s. One can modify that threshold by considering
;;             db-ratio * s instead of s (the default).
(define (gen-emp-rule db-ratio)
  (let* (;; Parameter
	 [db-ratio-n (Number db-ratio)]
	 ;; Variables
         [pattern (Variable "$pattern")]
         [db (Variable "$db")]
         [ms (Variable "$ms")]
         ;; Types
         [LambdaT (Type "LambdaLink")]
         [ConceptT (Type "ConceptNode")]
         [NumberT (Type "NumberNode")]
         ;; Vardecls
         [pattern-decl (TypedVariable pattern LambdaT)]
         [db-decl (TypedVariable db ConceptT)]
         [ms-decl (TypedVariable ms NumberT)]
         ;; Clauses
         [minsup-pattern (minsup-eval pattern db ms)])
  (Bind
    (VariableSet
      pattern-decl
      db-decl
      ms-decl)
    (And
      (Present minsup-pattern)
      (absolutely-true-eval minsup-pattern))
    (ExecutionOutput
      (GroundedSchema "scm-eager: emp-formula")
      (List
        ;; Conclusion
        (emp-eval pattern db)
	;; Premise
        minsup-pattern
	;; Parameter
	db-ratio-n)))))

(define (emp-formula conclusion . premises)
  ;; (cog-logger-debug "emp-formula conclusion = ~a, premises = ~a"
  ;;                   conclusion premises)
  (if (= (length premises) 2)
      (let* ([minsup-pattern (car premises)]
	     [db-ratio (cadr premises)]
             [pattern (get-pattern minsup-pattern)]
             [db (get-db minsup-pattern)]
             [emp-tv (cog-emp-tv pattern db db-ratio)])
        ;; (cog-logger-debug "emp-tv = ~a" emp-tv)
        (cog-set-tv! conclusion emp-tv))))
