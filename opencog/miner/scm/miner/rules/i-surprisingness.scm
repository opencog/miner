;; Calculate the I-Surprisingness as defined in
;; https://wiki.opencog.org/w/Measuring_Surprisingness of a pattern
;; composed of the conjunction of components.
;;
;; Semi-formally
;;
;; Evaluation (stv 1 1)
;;   Predicate "minsup"
;;   List
;;     Lambda
;;       <f-vardecl>
;;       And
;;         <cnj-body-1>
;;         ...
;;         <cnj-body-n>
;;     <db>
;;     <ms>
;; |-
;; Evaluation (stv 1 1)
;;   Predicate "isurp"
;;   List
;;     Lambda
;;       <f-vardecl>
;;       And
;;        <cnj-body-1>
;;        ...
;;        <cnj-body-n>
;;     <db>
;;     <isurp>

(load "miner-rule-utils.scm")

;; Generate a rule to calculate the I-Surprisingness of a pattern that
;; is the conjunction of nary components, given the following
;; arguments:
;;
;; - nary: the number of conjuncts of the pattern. It is assumed that
;;         1 < nary.
;;
;; - mode: variations of I-Surprisingness. Supported variations are
;;
;;         - 'isurp-old: verbatim port of Shujing I-Surprisingness.
;;
;;         - 'nisurp-old: verbatim port of Shujing normalized
;;                        I-Surprisingness.
;;
;;         - 'isurp: new implementation of I-Surprisingness (take
;;                   linkage into account).
;;
;;         - 'nisurp: new implementation of normalized
;;                    I-Surprisingness (take linkage into account).
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
(define (gen-i-surprisingness-rule mode nary db-ratio)
  ;; Shared variables
  (define f-vardecl (Variable "$f-vardecl"))
  (define db (Variable "$db"))
  (define ms (Variable "$ms"))
  (define db-ratio-n (Number db-ratio))
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
  ;; Formula
  (define formula-name (string-append "scm-eager: " (symbol->string mode) "-formula"))
  (define formula (GroundedSchema formula-name))

  (if (< 1 nary)
      (let* ((cnj-bodies (gen-variables "$cnj-bodies" nary))
             (f (Quote
                  (Lambda
                    (Unquote f-vardecl)
                    (Present
                      (map Unquote cnj-bodies)))))
             (f-minsup (minsup-eval f db ms))
             (f-isurp (surp-eval mode f db)))
        (Bind
          (VariableSet
            typed-f-vardecl
            cnj-bodies
            typed-db
            typed-ms)
          (And
            (Present
               f-minsup)
            (Absent
               f-isurp)
            (absolutely-true-eval f-minsup))
          (ExecutionOutput
            formula
            (List
	      ;; Conclusion
              f-isurp
	      ;; Premises
              f-minsup
	      ;; Parameters. TODO: maybe introduce a parameter link in
	      ;; the ure to avoid confounding with premises.
	      db-ratio-n))))))

;; I-Suprisingness formula
(define (gen-i-surprisingness-formula mode)
  (lambda (conclusion . premises)

    ;; (cog-logger-debug "(i-surprisingness-formula mode = ~a, conclusion = ~a, premises = ~a"
    ;;                   mode conclusion premises)

    (if (= 2 (length premises))
        (let* ((pat-isurp conclusion)
               (pat-minsup (car premises))
	       (db-ratio-n (cadr premises))
               (pat (get-pattern pat-minsup))
               (db (get-db pat-minsup))

               ;; Calculate I-Surprisingness of pat
               (isurp-op (cond ((equal? mode 'isurp-old) cog-isurp-old)
                               ((equal? mode 'nisurp-old) cog-nisurp-old)
                               ((equal? mode 'isurp) cog-isurp)
                               ((equal? mode 'nisurp) cog-nisurp)))
               (isurp (isurp-op pat db db-ratio-n)))
          (cog-set-tv! pat-isurp (stv isurp 1))))))

;; Instantiate isurp formula for the different modes
(define isurp-old-formula (gen-i-surprisingness-formula 'isurp-old))
(define nisurp-old-formula (gen-i-surprisingness-formula 'nisurp-old))
(define isurp-formula (gen-i-surprisingness-formula 'isurp))
(define nisurp-formula (gen-i-surprisingness-formula 'nisurp))
