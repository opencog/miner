;; Rule to calculate the Jensen-Shannon Distance between the empirical
;; truth value and the truth value estimate of a pattern according to
;; some db.
;;
;; Semi-formally
;;
;; Evaluation <emp-tv>
;;   Predicate "emp"
;;   List
;;     <pattern>
;;     <db>
;; Evaluation <est-tv>
;;   Predicate "est"
;;   List
;;     <pattern>
;;     <db>
;; |-
;; Evaluation <jsd-tv>
;;   Predicate "jsd"
;;   List
;;     <pattern>
;;     <db>

(load "miner-rule-utils.scm")

(define (gen-jsd-rule)
  (let* (;; Variables
         (pattern (Variable "$pattern"))
         (db (Variable "$db"))
         ;; Types
         (LambdaT (Type "LambdaLink"))
         (ConceptT (Type "ConceptNode"))
         ;; Vardecls
         (pattern-decl (TypedVariable pattern LambdaT))
         (db-decl (TypedVariable db ConceptT))
         ;; Clauses
         (emp (emp-eval pattern db))
         (est (est-eval pattern db)))
  (Bind
    (VariableSet
      pattern-decl
      db-decl)
    (And
      (Present
        emp
        est)
      (gt-zero-confidence-eval emp)
      (gt-zero-confidence-eval est))
    (ExecutionOutput
      (GroundedSchema "scm-eager: jsd-formula")
      (List
        (jsd-eval pattern db)
        emp
        est)))))

(define (jsd-formula conclusion . premises)
  (if (= (length premises) 2)
      (let* ((emp (list-ref premises 0))
             (est (list-ref premises 1))
             (pattern (get-pattern emp))
             (db (get-db emp))
             (jsd (cog-jsd (cog-tv emp) (cog-tv est)))
             (jsd-tv (stv jsd 1)))
        (cog-set-tv! conclusion jsd-tv))))
