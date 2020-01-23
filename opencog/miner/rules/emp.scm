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

(define (gen-emp-rule)
  (let* (;; Variables
         (pattern (Variable "$pattern"))
         (db (Variable "$db"))
         (ms (Variable "$ms"))
         ;; Types
         (LambdaT (Type "LambdaLink"))
         (ConceptT (Type "ConceptNode"))
         (NumberT (Type "NumberNode"))
         ;; Vardecls
         (pattern-decl (TypedVariable pattern LambdaT))
         (db-decl (TypedVariable db ConceptT))
         (ms-decl (TypedVariable ms NumberT))
         ;; Clauses
         (minsup-pattern (minsup-eval pattern db ms)))
  (Bind
    (VariableSet
      pattern-decl
      db-decl
      ms-decl)
    (And
      (Present minsup-pattern)
      (absolutely-true-eval minsup-pattern))
    (ExecutionOutput
      (GroundedSchema "scm: emp-formula")
      (List
        (emp-eval pattern db)
        minsup-pattern)))))

(define (emp-formula conclusion . premises)
  ;; (cog-logger-debug "emp-formula conclusion = ~a, premises = ~a"
  ;;                   conclusion premises)
  (if (= (length premises) 1)
      (let* ((minsup-pattern (car premises))
             (pattern (get-pattern minsup-pattern))
             (db (get-db minsup-pattern))
             (emp-tv (cog-emp-tv pattern db)))
        ;; (cog-logger-debug "emp-tv = ~a" emp-tv)
        (cog-set-tv! conclusion emp-tv))))
