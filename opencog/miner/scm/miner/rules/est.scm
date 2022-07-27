;; Rule to calculate the probability (or truth value) estimate of a
;; pattern, given the empirical probabilities (truth values) of its
;; components.
;;
;; Semi-formally
;;
;; Evaluation (stv 1 1)
;;   Predicate "minsup"
;;   List
;;     Lambda
;;       <vardecl>
;;       <body>
;;     <db>
;;     <ms>
;; |-
;; Evaluation <TV>
;;   Predicate "est"
;;   List
;;     Lambda
;;       <vardecl>
;;       <body>
;;     <db>
;;
;; TODO: in principle one would need to state the empirical
;; probabilities as premises as well, in practice however we ignore
;; that for now, and have the formula instead fetch them.

(load "miner-rule-utils.scm")

(define (gen-est-rule)
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
      (GroundedSchema "scm-eager: est-formula")
      (List
        (est-eval pattern db)
        minsup-pattern)))))

(define (est-formula conclusion . premises)
  ;; (cog-logger-debug "est-formula conclusion = ~a, premises = ~a"
  ;;                   conclusion premises)
  (if (= (length premises) 1)
      (let* ((minsup-pattern (car premises))
             (pattern (get-pattern minsup-pattern))
             (db (get-db minsup-pattern))
             (est-tv (cog-ji-tv-est pattern db)))
        ;; (cog-logger-debug "est-tv = ~a" est-tv)
        (cog-set-tv! conclusion est-tv))))
