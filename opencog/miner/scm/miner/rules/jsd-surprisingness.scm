;; Calculate the surprisingness of a pattern as the Jensen-Shannon
;; Distance between its empirical truth value and its truth value
;; estimate, based a reasoning (typically excluding the emp rule).
;;
;; Semi-formally
;;
;; Evaluation (stv 1 1)
;;   Predicate "minsup"
;;   List
;;     <patterm>
;;     <db>
;;     <ms>
;; Evaluation <jsd-tv>
;;   Predicate "jsd"
;;   List
;;     <patterm>
;;     <db>
;; |-
;; Evaluation <>
;;   Predicate "jsdsurp"
;;   List
;;     <pattern>
;;     <db>

(load "miner-rule-utils.scm")

;; Generate a rule to calculate the JSD-Surprisingness of a pattern
;; that is the conjunction of nary components.
;;
;; Assumption: 1 < nary
(define (gen-jsd-surprisingness-rule nary)
  ;; Shared variables
  (define f-vardecl (Variable "$f-vardecl"))
  (define db (Variable "$db"))
  (define ms (Variable "$ms"))
  ;; Types
  (define VariableT (Type "VariableNode"))
  (define VariableSetT (Type "VariableSet"))
  (define VariableListT (Type "VariableList"))
  (define varT (TypeChoice VariableT VariableSetT VariableListT))
  (define NumberT (Type "NumberNode"))
  (define ConceptT (Type "ConceptNode"))
  ;; Typed declations
  (define typed-f-vardecl (TypedVariable f-vardecl varT))
  (define typed-db (TypedVariable db ConceptT))
  (define typed-ms (TypedVariable ms NumberT))

  (if (< 1 nary)
      (let* ((cnj-bodies (gen-variables "$cnj-bodies" nary))
             (f (Quote
                  (Lambda
                    (Unquote f-vardecl)
                    (Present
                      (map Unquote cnj-bodies)))))
             (minsup-e (minsup-eval f db ms))
             (jsd-e (jsd-eval f db))
             (surp-e (surp-eval 'jsdsurp f db)))
        (Bind
          (VariableSet
            typed-f-vardecl
            cnj-bodies
            typed-db
            typed-ms)
          (And
            (Present
               minsup-e
               jsd-e)
            (Absent
               surp-e)
            (absolutely-true-eval minsup-e)
            (gt-zero-confidence-eval jsd-e))
          (ExecutionOutput
            (GroundedSchema "scm-eager: jsd-surprisingness-formula")
            (List
              surp-e
              minsup-e
              jsd-e))))))

;; I-Suprisingness formula
(define (jsd-surprisingness-formula conclusion . premises)

  ;; (cog-logger-debug "(jsd-surprisingness-formula mode = ~a, conclusion = ~a, premises = ~a"
  ;;                   mode conclusion premises)

  (if (= 2 (length premises))
      (let* ((jsdsurp-e conclusion)
             (minsup-e (list-ref premises 0))
             (jsd-e (list-ref premises 1))
             (pattern (get-pattern minsup-e))
             (db (get-db minsup-e))
             (surp (cog-mean jsd-e)))
        (cog-set-tv! jsdsurp-e (stv surp 1)))))
