;;
;; miner-utils.scm
;;
;;;; Commentary:
;;
;; Handy utilities for working with the ure pattern miner. In
;; particular to configure the rule engine.
;;
;; Utilities include:
;;
;; If you add more utilities don't forget to add them in the
;; export-miner-utils function.
;;
;;;; Code:
;; Copyright (c) 2018, OpenCog Foundation
;;

(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog ure))
(use-modules (opencog logger))
(use-modules (srfi srfi-1))

;; For some crazy reason I need to repaste absolutely-true here while
;; it is already defined in ure.
(define-public (absolutely-true A)
"
  Return TrueTV iff A's TV is TrueTV
"
  (bool->tv (tv->bool (cog-tv A))))

(define (mk-full-rule-path brf)
  (let ((rule-path "opencog/miner/rules/"))
    (string-append rule-path brf)))

;; Load here otherwise gen-shallow-specialization-rule is inaccessible
(load-from-path (mk-full-rule-path "shallow-specialization.scm"))

;; Load here otherwise gen-conjunction-expansion-rule is inaccessible
(load-from-path (mk-full-rule-path "conjunction-expansion.scm"))

;; Load here otherwise gen-i-surprisingness-rule is inaccessible
(load-from-path (mk-full-rule-path "i-surprisingness.scm"))

;; Load here otherwise gen-jsd-surprisingness-rule is inaccessible
(load-from-path (mk-full-rule-path "jsd-surprisingness.scm"))

;; Load here otherwise gen-emp-rule is inaccessible
(load-from-path (mk-full-rule-path "emp.scm"))

;; Load here otherwise gen-est-rule is inaccessible
(load-from-path (mk-full-rule-path "est.scm"))

;; Load here otherwise gen-jsd-rule is inaccessible
(load-from-path (mk-full-rule-path "jsd.scm"))

(define (iota-plus-one x)
"
  Like iota but goes from 1 to x included instead of going
  from 0 to x excluded.
"
  (map (lambda (x) (+ x 1)) (iota x)))

(define (greater-tv-strength x y)
"
  Return #t if the tv strength of x is greater than that of y
"
  (> (cog-mean x) (cog-mean y)))

(define (desc-sort-by-tv-strength lst)
"
  Given a list of atoms, sort these atom in descending order according
  to their tv strengths.
"
  (sort lst greater-tv-strength))

(define (top)
"
  Insert the top abstraction in the current atomspace

  (Lambda (Variable \"$X\") (Variable \"$X\"))
"
  (let ((top-arg (Variable "$top-arg")))
    (Lambda top-arg (Present top-arg))))

(define (random-db-cpt)
"
  Create a random Concept node for adding data tree members
"
  (random-node 'ConceptNode 16 "db-"))

(define (random-miner-rbs-cpt)
"
  Create a random Concept node for defining a pattern miner rule base
"
  (random-node 'ConceptNode 16 "pattern-miner-rbs-"))

(define (random-surprisingness-rbs-cpt)
"
  Create a random Concept node for defining a rule base for surprisingness
"
  (random-node 'ConceptNode 16 "surprisingness-rbs-"))

(define (fill-db-cpt db-cpt db)
"
  Usage: (fill-db-cpt db-cpt db)

  For each element dt of db create

  MemberLink
    dt
    db-cpt

  db can be
  1. a Scheme list
  2. an Atomese List or Set
  3. an AtomSpace

  Once all memberships have been added to the current atomspace,
  db-cpt is returned.
"
  (define (is-List-Set a)
    (and (cog-atom? db)
         (or (eq? (cog-type? db) 'ListLink)
             (eq? (cog-type? db) 'SetLink))))
  (let* ((db-lst (cond ;; Scheme list
                       ((list? db) db)
                       ;; Atomese List or Set
                       ((is-List-Set db) (cog-outgoing-set db))
                       ;; AtomSpace
                       ;; TODO: bug!!! should use the given atomspace
                       ((cog-atomspace? db) (cog-get-atoms 'Atom #t))))
         (mk-member (lambda (dt) (Member dt db-cpt))))
    (for-each mk-member db-lst))
  db-cpt)

(define (configure-mandatory-rules pm-rbs)
  ;; Maybe remove, nothing is mandatory anymore
  *unspecified*)

(define (configure-shallow-specialization-rule pm-rbs unary max-variables)
  (define mv (min 9 max-variables))

  ;; Load and associate mandatory rules to pm-rbs
  ;; (load-from-path (mk-full-rule-path "shallow-specialization.scm"))
  (let* ((rule-name (string-append "shallow-specialization-"
                                   (if unary "unary-" "")
                                   "mv-" (number->string mv)
                                   "-rule"))
         (rule-alias (DefinedSchema rule-name))
         (rule-tv (stv 0.9 1)))
    ;; Add definition
    (DefineLink rule-alias (gen-shallow-specialization-rule unary mv))
    (ure-add-rule pm-rbs rule-alias rule-tv)))

(define (configure-conjunction-expansion-rules pm-rbs
                                               conjunction-expansion
                                               enforce-specialization
                                               max-conjuncts
                                               max-variables)
  (define mv (min 9 max-variables))

  (if (and conjunction-expansion (< 1 max-conjuncts))
      ;; (load-from-path (mk-full-rule-path "conjunction-expansion.scm"))
      (let* ((namify (lambda (i)
                       (string-append "conjunction-expansion-"
                                      (if enforce-specialization
                                          "specialization-" "")
                                      (if (< 0 i)
                                          (string-append (number->string i) "ary-") "")
                                      "mv-" (number->string mv)
                                      "-rule")))
             (aliasify (lambda (i) (DefinedSchema (namify i))))
             (definify (lambda (i)
                         (DefineLink
                           (aliasify i)
                           (gen-conjunction-expansion-rule i mv enforce-specialization))))
             (conf 1)    ; TODO: make this use configurable
             ;; The more arity the expansion the lower its
             ;; priority. Makes sure it's priority is also below that
             ;; of shallow specialization.
             (tvfy (lambda (i) (stv (* 0.5 (- 1 (* 0.1 i))) conf)))
             (rulify (lambda (i) (definify i) (list (aliasify i) (tvfy i))))
             (rules (if (or (<= max-conjuncts 0) (< 9 max-conjuncts))
                        ;; No maximum conjuncts
                        (list (rulify 0))
                        ;; At most max-conjuncts conjuncts
                        (map rulify (iota-plus-one (- max-conjuncts 1))))))
        (ure-add-rules pm-rbs rules))))

(define (false-tv? tv)
  (equal? tv (stv 0 1)))

(define* (configure-optional-rules pm-rbs
                                   #:key
                                   (conjunction-expansion #t)
                                   (enforce-specialization #t)
                                   (max-conjuncts 3)
                                   (max-variables 3))

  ;; Load shallow specialization, either unary, if
  ;; conjunction-expansion is enabled, or not
  (configure-shallow-specialization-rule pm-rbs
                                         conjunction-expansion
                                         max-variables)

  ;; Load conjunction-expansion and associate to pm-rbs
  (configure-conjunction-expansion-rules pm-rbs
                                         conjunction-expansion
                                         enforce-specialization
                                         max-conjuncts
                                         max-variables))

(define* (configure-rules pm-rbs
                          #:key
                          (conjunction-expansion #t)
                          (enforce-specialization #t)
                          (max-conjuncts 3)
                          (max-variables 3))
  (configure-mandatory-rules pm-rbs)
  (configure-optional-rules pm-rbs
                            #:conjunction-expansion conjunction-expansion
                            #:enforce-specialization enforce-specialization
                            #:max-conjuncts max-conjuncts
                            #:max-variables max-variables))

(define* (configure-surprisingness surp-rbs mode max-conjuncts)
  ;; Add surprisingness rules
  (let* ((namify (lambda (i) (string-append (symbol->string mode) "-"
                                            (number->string i)
                                            "ary-rule")))
         (aliasify (lambda (i) (DefinedSchema (namify i))))
         (rule-gen (cond ((or (eq? mode 'isurp-old)
                              (eq? mode 'nisurp-old)
                              (eq? mode 'isurp)
                              (eq? mode 'nisurp))
                          (lambda (i) (gen-i-surprisingness-rule mode i)))
                         ((eq? mode 'jsdsurp) gen-jsd-surprisingness-rule)))
         (definify (lambda (i) (DefineLink
                                 (aliasify i)
                                 (rule-gen i))))
         (rulify (lambda (i) (definify i) (aliasify i)))
         (rules (map rulify (cdr (iota-plus-one max-conjuncts)))))
    (ure-add-rules surp-rbs rules))

  ;; In case of jsdsurp we also need emp, est and jsd rules
  (if (eq? mode 'jsdsurp)
      (let* ((emp-alias (DefinedSchema "emp-rule"))
             (est-alias (DefinedSchema "est-rule"))
             (jsd-alias (DefinedSchema "jsd-rule"))
             (emp-def (Define emp-alias (gen-emp-rule)))
             (est-def (Define est-alias (gen-est-rule)))
             (jsd-def (Define jsd-alias (gen-jsd-rule))))
        (ure-add-rules surp-rbs (list emp-alias est-alias jsd-alias)))))

(define (pattern-var)
  (Variable "$pattern"))

(define (surp-target mode db-cpt)
  (surp-eval mode (pattern-var) db-cpt))

(define (surp-vardecl)
  (TypedVariable (pattern-var) (Type "LambdaLink")))

(define* (configure-miner pm-rbs
                          #:key
                          (maximum-iterations 1000)
                          (complexity-penalty 1)
                          (conjunction-expansion #t)
                          (enforce-specialization #t)
                          (max-conjuncts 3)
                          (max-variables 3))
"
  Given a Concept node representing a rule based system for the
  pattern miner. Automatically configure it with the appropriate
  rules and parameters.

  Usage: (configure-miner pm-rbs
                          #:maximum-iterations mi
                          #:complexity-penalty cp
                          #:conjunction-expansion ce
                          #:enforce-specialization es
                          #:max-conjuncts mc)
                          #:max-variables mv)

  pm-rbs: Concept node of the rule-based system to configure

  mi: [optional, default=1000] Maximum number of iterations allocated.
      If negative then the pattern miner keeps running till all patterns
      have been exhausted (not recommended unless you know what you're doing).

  cp: [optional, default=1] Complexity penalty parameter passed to the forward
      chainer. It controls breadth vs depth search. A high value means more
      breadth. A value of 0 means a equilibrium between breadth and depth.
      A negative value means more depth. Possible range is (-inf, +inf)
      but it's rarely necessary in practice to go outside of [-10, 10].

  ce: [optional, default=#t] Flag whether to use the conjunction expansion
      heuristic rules. It will only expand conjunctions with enough support
      with patterns with enough support.

  es: [optional, default=#t] Flag whether specialization is enforced.
      Some rules such as conjunction expansion can create abstractions
      due to having more variables, this flag enforces that only
      specializations will be created.

  mc: [optional, default=3] In case ce is set to #t, and thus incremental
      conjunction expansion is enabled, that option allows to limit the number
      of conjuncts to mc. If negative then the number of conjuncts can grow
      unlimited (not recommended unless you know what you're doing). As of
      now mc can not be set above 9 (which should be more than enough).

  mv: [optional, default=3] Maximum number of variables that the resulting
      patterns should contain. As of now mv cannot be set above 9 (which
      should be more than enough).
"
  ;; Load and associate rules to pm-rbs
  (configure-rules pm-rbs
                   #:conjunction-expansion conjunction-expansion
                   #:enforce-specialization enforce-specialization
                   #:max-conjuncts max-conjuncts
                   #:max-variables max-variables)

  ;; Set parameters
  (ure-set-maximum-iterations pm-rbs maximum-iterations)
  (ure-set-complexity-penalty pm-rbs complexity-penalty)

  ;; If there is no conjunction expansion then each rule is
  ;; deterministic, thus no need to retry exhausted sources
  (ure-set-fc-retry-exhausted-sources pm-rbs (and conjunction-expansion
                                                  (< 1 max-conjuncts))))

(define (minsup-eval pattern db ms)
"
  Construct

  Evaluation
    Predicate \"minsup\"
    List
      pattern
      db
      ms
"
  (Evaluation
     (Predicate "minsup")
     (List
        pattern
        db
        (if (number? ms) (Number ms) ms))))

(define (minsup-eval-true pattern db ms)
"
  Like minsup-eval and add (stv 1 1) on the EvaluationLink
"
  (cog-set-tv! (minsup-eval pattern db ms) (stv 1 1)))

(define (surp-eval mode pattern db)
"
  Construct

  Evaluation
    Predicate \"mode\"
    List
      pattern
      db

  where mode can be 'isurp-old, 'nisurp-old, 'isurp, 'nisurp, 'jsdsurp.
"
  (Evaluation
    (Predicate (symbol->string mode))
    (List
      pattern
      db)))

(define (get-members C)
"
  Given a concept node C, return all its members
"
  (let* ((member-links (cog-filter 'MemberLink (cog-incoming-set C)))
         (member-of-C (lambda (x) (equal? C (gdr x))))
         (members (map gar (filter member-of-C member-links))))
    members))

(define (get-cardinality C)
"
  Giveb a concept node C, return its number of members
"
  (length (get-members C)))

(define (size-ge db ms)
  (let* ((result (>= (get-cardinality db) (cog-number ms))))
    (bool->tv result)))

(define (db->atomspace db)
"
  Create an atomspace with all members of concept db in it.
"
  (let* ((members (get-members db))
         (db-as (cog-new-atomspace)))
    (cog-cp db-as members)
    db-as))

(define (pattern->bindlink pattern)
"
  Turn a pattern into a BindLink for subsequent pattern
  matching db.
"
  (if (= (cog-arity pattern) 2)
      ;; With variable declaration
      (let* ((vardecl (gar pattern))
             (body (gdr pattern)))
        (Bind vardecl body body)) ; to deal with unordered links
      ;; Without variable declaration
      (let* ((body (gar pattern)))
        (Bind body body)))) ; to deal with unordered links

(define (fetch-patterns db ms)
"
  Fetch all patterns with enough support, thus found in the following
  hypergraphs

  Evaluation (stv 1 1)
    Predicate \"minsup\"
    List
      <pattern>
      db
      ms
"
  (let* ((patvar (Variable "$patvar"))
         (target (minsup-eval patvar db ms))
         (vardecl (TypedVariable patvar (Type "LambdaLink")))
         (precond (absolutely-true-eval target))
         (gl (Get vardecl (And (Present target) precond))))
    (cog-execute! gl)))

(define* (conjunct-pattern nconj)
"
  Create a pattern of nconj conjunctions.

  For instance (conjunct-pattern 3), creates

  (Lambda
    (VariableList
      (Variable \"$X-1\")
      (Variable \"$X-2\")
      (Variable \"$X-3\"))
    (Present
      (Variable \"$X-1\")
      (Variable \"$X-2\")
      (Variable \"$X-3\")))
"
  (let* ((vars (gen-variables "$X" nconj))
         (var-lst (VariableList vars))
         (var-conj (Present vars)))
    (Lambda var-lst var-conj)))

(define* (cog-miner . args)
  (display ("The command you are looking for is cog-mine.")))

(define* (cog-mine db
                   #:key
                   (minsup 10)
                   (initpat (top))
                   (maximum-iterations 1000)
                   (complexity-penalty 1)
                   (conjunction-expansion #t)
                   (enforce-specialization #t)
                   (max-conjuncts 3)
                   (max-variables 3)
                   (surprisingness 'isurp))
"
  Mine patterns in db (data trees, a.k.a. grounded hypergraphs) with minimum
  support ms, optionally using mi iterations and starting from the initial
  pattern initpat.

  Usage: (cog-mine db
                   #:minsup ms
                   #:initpat ip
                   #:maximum-iterations mi
                   #:complexity-penalty cp
                   #:conjunction-expansion ce
                   #:enforce-specializtion es
                   #:max-conjuncts mc
                   #:max-variables mv
                   #:surprisingness su)

  db: Collection of data trees to mine. It can be given in 3 forms

         1. Scheme list of atoms

            (list t1 ... tn)

         2. Atomese list or set of atoms

            (List t1 ... tn)
            (Set t1 ... tn)

         3. A concept with all the data trees in it

            (Concept db-name)

            such that

            (Member
              t1
              (Concept db-name))
            ...
            (Member
              tn
              (Concept db-name))

  ms: [optional, default=10] Minimum support. All patterns with frequency below
      ms are discarded. Can be a Scheme number or an Atomese number node.

  ip: [optional, default=(top)] Initial pattern to start the search from.
      All mined patterns will be specializations of this pattern.

  mi: [optional, default=1000] Maximum number of iterations allocated.
      If negative then the pattern miner keeps running till all patterns
      have been exhausted (not recommended unless you know what you're doing).

  cp: [optional, default=1] Complexity penalty parameter passed to the forward
      chainer. It controls breadth vs depth search. A high value means more
      breadth. A value of 0 means an equilibrium between breadth and depth.
      A negative value means more depth. Possible range is (-inf, +inf)
      but it's rarely necessary in practice to go outside of [-10, 10].

  ce: [optional, default=#t] Flag whether to use the conjunctions expansion
      heuristic rules. It will only expand conjunctions with enough support
      with patterns with enough support.

  es: [optional, default=#t] Flag whether specialization is enforced.
      Some rules such as conjunction expansion can create abstractions
      due to having more variables, this flag enforces that only
      specializations will be created.

  mc: [optional, default=3] In case tv is set to a positive strength and
      confidence, and thus incremental conjunction expansion is enabled, that
      option allows to limit the number of conjuncts to mc. If negative then
      the number of conjuncts can grow unlimited (not recommended unless you
      know what you're doing). As of now mc cannot be set above 9 (which
      should be more than enough).

  mv: [optional, default=3] Maximum number of variables that the resulting
      patterns can contain. As of now mv cannot be set above 9 (which
      should be more than enough).

  su: [optional, default='isurp] After running the pattern miner,
      patterns can be ranked according to some surprisingness measure.
      The following surported modes are:

      'isurp-old:  Verbatim port of Shujing I-Surprisingness.

      'nisurp-old: Verbatim port of Shujing normalized I-Surprisingness.

      'isurp:      New implementation of I-Surprisingness that takes
                   linkage into account.

      'nisurp:     New implementation of normalized I-Surprisingness
                   that takes linkage into account.

      'jsdsurp:    Jensen-Shannon Distance based surprisingness.
                   The type of surprisingness is determined by the way
                   the truth value estimate is calculated.

      'none:       No surprisingness measure is applied.

  Under the hood it will create a rule base and a query for the rule
  engine, configure it according to the user's options and run it.
  Everything takes place in a child atomspace. After the job is done
  it will remove the child atomspace after copying the solution set
  in the parent atomspace.

  Pattern mining is computationally demanding. There are three ways
  to improve performances at this time.

  1. Set ms as high as possible. The higher the minium support the
     more pruning will take place in search tree. That is because
     specializations cannot have more support than their parent
     abstraction.

  2. If it takes too long to complete, it means the search tree is
     too large to explore entirely. Lower the number of iterations
     of the rule engine, mi, to halt the exploration earlier.

  3. If you have any idea of the kind of patterns you are looking
     for, you can provide an initial pattern, ip. All mined patterns
     will be specialized from that pattern. This can considerably
     reduce the search space as only a subtree of the whole search
     tree is considered.

  4. If your pattern is a conjunction of multiple clauses, you can
     enable incremental conjunction expansion, see the
     #:conjunction-expansion option.
"
  (let* (;; Create a temporary child atomspace for the URE
         (tmp-as (cog-new-atomspace (cog-atomspace)))
         (parent-as (cog-set-atomspace! tmp-as))
         (db-concept? (and (cog-atom? db)
                           (eq? (cog-type db) 'ConceptNode)))
         (db-cpt (if (not db-concept?)
                     ;; Construct a temporary concept containing
                     ;; the db
                     (fill-db-cpt (random-db-cpt) db)
                     ;; Otherwise db is already a concept
                     db))
         (ms-nn (if (number? minsup) (Number minsup) minsup))
         ;; Check that the initial pattern has enough support
         (es (cog-enough-support? initpat db-cpt ms-nn)))
    (if (not es)
        ;; The initial pattern doesn't have enough support, thus the
        ;; solution set is empty.
        (begin (cog-set-atomspace! parent-as)
               ;; TODO: delete tmp-as
               (Set))

        ;; The initial pattern has enough support, let's configure the
        ;; rule engine and run the pattern mining query
        (let* (;; Configure pattern miner forward chainer
               (source (minsup-eval-true initpat db-cpt minsup))
               (miner-rbs (random-miner-rbs-cpt))
               (cfg-m (configure-miner miner-rbs
                                       #:maximum-iterations maximum-iterations
                                       #:complexity-penalty complexity-penalty
                                       #:conjunction-expansion conjunction-expansion
                                       #:enforce-specialization enforce-specialization
                                       #:max-conjuncts max-conjuncts
                                       #:max-variables max-variables))

               ;; Run pattern miner in a forward way
               (results (cog-fc miner-rbs source))
               ;; Fetch all relevant results
               (patterns (fetch-patterns db-cpt minsup))
               (patterns-lst (cog-outgoing-set patterns)))

          (if (equal? surprisingness 'none)

              ;; No surprisingness, simple return the pattern list
              (let* ((parent-patterns-lst (cog-cp parent-as patterns-lst)))
                (cog-set-atomspace! parent-as)
                parent-patterns-lst)

              ;; Run surprisingness
              (let*
                  ;; Configure surprisingness backward chainer
                  ((surp-rbs (random-surprisingness-rbs-cpt))
                   (target (surp-target surprisingness db-cpt))
                   (vardecl (surp-vardecl))
                   (cfg-s (configure-surprisingness surp-rbs
                                                    surprisingness
                                                    max-conjuncts))

                   ;; Run surprisingness in a backward way
                   (surp-res (cog-bc surp-rbs target #:vardecl vardecl))
                   (surp-res-lst (cog-outgoing-set surp-res))
                   (surp-res-sort-lst (desc-sort-by-tv-strength surp-res-lst))

                   ;; Copy the results to the parent atomspace
                   (parent-surp-res (cog-cp parent-as surp-res-sort-lst)))
                (cog-set-atomspace! parent-as)
                parent-surp-res))))))

(define (export-miner-utils)
  (export
    iota-plus-one
    top
    random-db-cpt
    random-miner-rbs-cpt
    fill-db-cpt
    configure-mandatory-rules
    configure-optional-rules
    configure-rules
    configure-surprisingness
    configure-miner
    minsup-eval
    minsup-eval-true
    surp-eval
    get-members
    get-cardinality
    fetch-patterns
    conjunct-pattern
    cog-miner
    cog-mine
    ;; Functions to allow the rules to run
    shallow-specialization-mv-1-formula
    shallow-specialization-mv-2-formula
    shallow-specialization-mv-3-formula
    shallow-specialization-mv-4-formula
    shallow-specialization-mv-5-formula
    shallow-specialization-mv-6-formula
    shallow-specialization-mv-7-formula
    shallow-specialization-mv-8-formula
    shallow-specialization-mv-9-formula
    conjunction-expansion-mv-1-formula
    conjunction-expansion-mv-2-formula
    conjunction-expansion-mv-3-formula
    conjunction-expansion-mv-4-formula
    conjunction-expansion-mv-5-formula
    conjunction-expansion-mv-6-formula
    conjunction-expansion-mv-7-formula
    conjunction-expansion-mv-8-formula
    conjunction-expansion-mv-9-formula
    conjunction-expansion-specialization-mv-1-formula
    conjunction-expansion-specialization-mv-2-formula
    conjunction-expansion-specialization-mv-3-formula
    conjunction-expansion-specialization-mv-4-formula
    conjunction-expansion-specialization-mv-5-formula
    conjunction-expansion-specialization-mv-6-formula
    conjunction-expansion-specialization-mv-7-formula
    conjunction-expansion-specialization-mv-8-formula
    conjunction-expansion-specialization-mv-9-formula
    isurp-old-formula
    nisurp-old-formula
    isurp-formula
    nisurp-formula
    emp-formula
    est-formula
    jsd-formula
    jsd-surprisingness-formula
    unary-conjunction
    unary-conjunction-pattern
  )
)
