;; Rule to expand a conjunction with an existing pattern given that
;; both the conjunction and the existing pattern have enough support.
;;
;; Semi-formally
;;
;; Evaluation (stv 1 1)
;;   Predicate "minsup"
;;   List
;;     Lambda
;;       <f-vardecl>
;;       <f-body>
;;     <db>
;;     <ms>
;; Evaluation (stv 1 1)
;;   Predicate "minsup"
;;   List
;;     Lambda
;;       <g-vardecl>
;;       <g-body>
;;     <db>
;;     <ms>
;; |-
;; Evaluation (stv 1 1)
;;   Predicate "minsup"
;;   List
;;     Lambda
;;       VariableSet
;;         <f-vardecl>
;;         <g-vardecl>
;;       And
;;         <f-body>
;;         <g-body>
;;     <db>
;;     <ms>
;;
;; Where <f-body> may be a conjunction of multiple conjuncts, while
;; <g-body> may only be a single conjunct. Also it is assumed that
;; variables are properly alpha-converted to avoid any variable name
;; collision. Finally,
;;
;; VariableSet
;;   <f-vardecl>
;;   <g-vardecl>
;;
;; and
;;
;; And
;;   <f-body>
;;   <g-body>
;;
;; are flattened. Also the resulting variable list may not include all
;; the variables, that is in practice all variable specializations are
;; considered. There is a also a flag specialization to discard any
;; extra variable as to enforce that the conjunction expansion is not
;; creating abstractions.

(load "miner-rule-utils.scm")

;; Generate a conjunction expansion rule so that the left pattern, f,
;; is a nary conjunction. Lets call f the left conjunction. Thus the
;; produced pattern, will be a nary+1 conjunction, and the right
;; pattern, g, is a unary conjunction.
;;
;; * nary <= 0: generate a rule that doesn't restrict the arity of the
;;              left conjunction.
;;
;; * 0 < nary: generate a rule that restricts the arity of the left
;;             conjunction to nary.
;;
;; * mv is the maximum of variables allowed in the resulting patterns.
;;
;; * enforce-specialization is a flag to enforce specialization by not
;;                          allowing extra variables in the expansion.
(define (gen-conjunction-expansion-rule nary mv enforce-specialization)
  ;; Shared variables
  (define f-vardecl (Variable "$f-vardecl"))
  (define g-vardecl (Variable "$g-vardecl"))
  (define db (Variable "$db"))
  (define ms (Variable "$ms"))
  (define g-body (Variable "$g-body"))
  ;; Shared types
  (define ConceptT (Type "ConceptNode"))
  (define NumberT (Type "NumberNode"))
  (define AndT (Type "AndLink"))
  ;; Shared variable declarations
  (define f-vardecl-decl f-vardecl)
  (define g-vardecl-decl g-vardecl)
  (define g-body-decl g-body)
  (define db-decl (TypedVariable db ConceptT))
  (define ms-decl (TypedVariable ms NumberT))
  ;; Shared clauses
  (define g (Quote (Lambda (Unquote g-vardecl) (Unquote g-body))))
  (define minsup-g (minsup-eval g db ms))
  ;; Formula
  (define formula-name (string-append "scm-eager: conjunction-expansion-"
                                      (if enforce-specialization
                                          "specialization-" "")
                                      "mv-" (number->string mv)
                                      "-formula"))
  (define formula (GroundedSchema formula-name))

  ;; Generate rule by nary cases
  (if (<= nary 1)
      ;; There are 2 cases
      ;; 1. no restriction on the arity of the left conjunction,
      ;; 2. unary left conjunction
      (let* (;; Variables
             (f-body (Variable "$f-body"))
             ;; Vardecls
             (f-body-decl f-body)
             ;; Clauses
             (f (Quote (Lambda (Unquote f-vardecl) (Unquote f-body))))
             (minsup-f (minsup-eval f db ms)))
        (Bind
          (VariableSet
            f-vardecl-decl
            g-vardecl-decl
            f-body-decl
            g-body-decl
            db-decl
            ms-decl)
          (And
            (Present
              minsup-f
              minsup-g)
            (absolutely-true-eval minsup-f)
            (absolutely-true-eval minsup-g)
            (not-equal-top f)
            (not-equal-top g)
            (unary-conjunction-eval g-body)
            ;; If nary equals to 1, make sure the left conjunction is unary
            (if (= nary 1)
                (unary-conjunction-eval f-body)
                '()))
          (ExecutionOutput
            formula
            (List
              ;; Fake conclusion, since we can't statically define its
              ;; pattern ATM
              (minsup-eval (top) db ms)
              ;; Premises, wrap in Set because their order
              ;; does not matter
              (Set minsup-f
                   minsup-g)))))

      ;; Left pattern has 1 < nary arity
      (let* (;; Variables
             (f-conjuncts (gen-variables "$f-conjunct" nary))
             ;; Vardecls
             (f-conjuncts-decls f-conjuncts)
             ;; clauses
             (f (Quote (Lambda (Unquote f-vardecl)
                               (Present (map Unquote f-conjuncts)))))
             (minsup-f (minsup-eval f db ms)))
      (Bind
        (VariableSet
          f-vardecl-decl
          g-vardecl-decl
          f-conjuncts-decls
          g-body-decl
          db-decl
          ms-decl)
        (And
          (Present
            minsup-f
            minsup-g)
          (absolutely-true-eval minsup-f)
          (absolutely-true-eval minsup-g)
          (not-equal-top g)
          (unary-conjunction-eval g-body))
        (ExecutionOutput
          formula
          (List
            ;; Fake conclusion, since we can't statically define its
            ;; pattern ATM
            (minsup-eval (top) db ms)
            ;; Premises, wrap in Set because their order
            ;; does not matter
            (Set minsup-f
                 minsup-g)))))))

;; Conjunction expansion formula
(define (gen-conjunction-expansion-formula mv enforce-specialization)
  (lambda (conclusion . premises)
    ;; (cog-logger-debug "conjunction-expansion-formula mv = ~a, conclusion = ~a, premises = ~a" mv conclusion premises)
    (if (= (length premises) 1)
        (let* ((minsup-fg (car premises))
               (minsup-f (cog-outgoing-atom minsup-fg 0))
               (minsup-g (cog-outgoing-atom minsup-fg 1))
               (f (get-pattern minsup-f))
               (g (get-pattern minsup-g))
               (db (get-db minsup-f))
               (ms (get-ms minsup-f))
               (mv-nn (Number mv))
               (es enforce-specialization)
               ;; Swap f and g to make sure the second argument of
               ;; cog-expand-conjunction is never a conjunction
               (fgs (if (unary-conjunction? (get-body g))
                        (cog-expand-conjunction f g db ms mv-nn es)
                        (cog-expand-conjunction g f db ms mv-nn es)))
               (mk-minsup (lambda (fg) (minsup-eval-true fg db ms)))
               ;; cog-expand-conjunction only return patterns with
               ;; enough support
               (minsup-fgs (map mk-minsup (cog-outgoing-set fgs))))
          (Set minsup-fgs)))))

;; Instantiate conjunction expansion formulae for different maximum
;; number of variables
(define (gen-formula a) (gen-conjunction-expansion-formula a #f))
(define conjunction-expansion-mv-1-formula (gen-formula 1))
(define conjunction-expansion-mv-2-formula (gen-formula 2))
(define conjunction-expansion-mv-3-formula (gen-formula 3))
(define conjunction-expansion-mv-4-formula (gen-formula 4))
(define conjunction-expansion-mv-5-formula (gen-formula 5))
(define conjunction-expansion-mv-6-formula (gen-formula 6))
(define conjunction-expansion-mv-7-formula (gen-formula 7))
(define conjunction-expansion-mv-8-formula (gen-formula 8))
(define conjunction-expansion-mv-9-formula (gen-formula 9))

;; Same as above but with enforced specialization
(define (gen-spec-formula a) (gen-conjunction-expansion-formula a #t))
(define conjunction-expansion-specialization-mv-1-formula (gen-spec-formula 1))
(define conjunction-expansion-specialization-mv-2-formula (gen-spec-formula 2))
(define conjunction-expansion-specialization-mv-3-formula (gen-spec-formula 3))
(define conjunction-expansion-specialization-mv-4-formula (gen-spec-formula 4))
(define conjunction-expansion-specialization-mv-5-formula (gen-spec-formula 5))
(define conjunction-expansion-specialization-mv-6-formula (gen-spec-formula 6))
(define conjunction-expansion-specialization-mv-7-formula (gen-spec-formula 7))
(define conjunction-expansion-specialization-mv-8-formula (gen-spec-formula 8))
(define conjunction-expansion-specialization-mv-9-formula (gen-spec-formula 9))
