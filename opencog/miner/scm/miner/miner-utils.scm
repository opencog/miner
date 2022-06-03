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

;; Place this helper on top because it is used by
;; default-initial-pattern
(define (top)
"
  Insert the top abstraction in the current atomspace

  (Lambda (Variable \"$X\") (Variable \"$X\"))
"
  (let ((top-arg (Variable "$top-arg")))
    (Lambda top-arg (Present top-arg))))

;; Default parameter values
(define default-minimum-support 10)
(define default-minimum-frequency -1)
(define default-initial-pattern (top))
(define default-jobs 1)
(define default-maximum-iterations 100)
(define default-complexity-penalty 1)
(define default-conjunction-expansion #t)
(define default-enforce-specialization #t)
(define default-maximum-conjuncts 3)
(define default-maximum-variables 3)
(define default-maximum-spcial-conjuncts 1)
(define default-maximum-cnjexp-variables 2)
(define default-surprisingness 'isurp)
(define default-db-ratio 1)
(define default-enable-type #f)
(define default-enable-glob #f)
(define default-ignore-variables '())

;; For some crazy reason I need to repaste absolutely-true here while
;; it is already defined in ure.
(define (absolutely-true A)
"
  Return TrueTV iff A's TV is TrueTV
"
  (bool->tv (tv->bool (cog-tv A))))

(define (mk-full-rule-path brf)
  (let ((rule-path "opencog/miner/"))
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

(define (get-db-lst db)
"
  Given a db which either
  1. a Scheme list
  2. an Atomese List or Set
  3. an AtomSpace

  return a scheme list of all the atoms in that db.
"
  (define (lst-set? a)
    (and (cog-atom? db)
         (or (eq? (cog-type? db) 'ListLink)
             (eq? (cog-type? db) 'SetLink))))
  (cond ;; Scheme list
        ((list? db) db)
        ;; Atomese List or Set
        ((lst-set? db) (cog-outgoing-set db))
        ;; AtomSpace
        ((cog-atomspace? db) (let* ((prev-as (cog-set-atomspace! db))
                                    (db-atoms (cog-get-atoms 'Atom #t)))
                               (cog-set-atomspace! prev-as)
                               db-atoms))))

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
  (let* ((db-lst (get-db-lst db))
         (mk-member (lambda (dt) (Member dt db-cpt))))
    (for-each mk-member db-lst))
  db-cpt)

(define (configure-mandatory-rules pm-rbs)
  ;; Maybe remove, nothing is mandatory anymore
  *unspecified*)

(define (configure-shallow-specialization-rules pm-rbs
                                                maximum-variables
                                                maximum-spcial-conjuncts
                                                enable-type
                                                enable-glob
                                                ignore-variables)
  (define mv (min 9 maximum-variables))

  ;; Only define such rules if maximum-spcial-conjuncts if above 0
  (if (< 0 maximum-spcial-conjuncts)

      ;; Load and associate mandatory rules to pm-rbs
      ;; (load-from-path (mk-full-rule-path "shallow-specialization.scm"))
      (let* ((namify (lambda (i)
                       (string-append "shallow-specialization-"
                                      (string-append (number->string i) "ary-")
                                      "mv-" (number->string mv)
                                      "-rule")))
             (aliasify (lambda (i) (DefinedSchema (namify i))))
             (definify (lambda (i)
                         (DefineLink
                           (aliasify i)
                           (gen-shallow-specialization-rule i mv enable-type enable-glob ignore-variables))))
             (rule-tv (stv 0.9 1))
             (rulify (lambda (i) (definify i) (list (aliasify i) rule-tv)))
             (rules (map rulify (iota-plus-one maximum-spcial-conjuncts))))
        (ure-add-rules pm-rbs rules))))

(define (configure-conjunction-expansion-rules pm-rbs
                                               conjunction-expansion
                                               enforce-specialization
                                               maximum-conjuncts
                                               maximum-cnjexp-variables)
  (define mv (min 9 maximum-cnjexp-variables))

  (if (and conjunction-expansion (< 1 maximum-conjuncts))
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
             (conf 1)    ; TODO: make this user configurable
             ;; The more arity the expansion the lower its
             ;; priority. Makes sure it's priority is also below that
             ;; of shallow specialization.
             (tvfy (lambda (i) (stv (* 0.5 (- 1 (* 0.1 i))) conf)))
             (rulify (lambda (i) (definify i) (list (aliasify i) (tvfy i))))
             (rules (if (or (<= maximum-conjuncts 0) (< 9 maximum-conjuncts))
                        ;; No maximum conjuncts
                        (list (rulify 0))
                        ;; At most maximum-conjuncts conjuncts
                        (map rulify (iota-plus-one (- maximum-conjuncts 1))))))
        (ure-add-rules pm-rbs rules))))

(define (false-tv? tv)
  (equal? tv (stv 0 1)))

(define* (configure-optional-rules pm-rbs
                                   #:key
                                   (conjunction-expansion default-conjunction-expansion)
                                   (enforce-specialization default-enforce-specialization)
                                   (maximum-conjuncts default-maximum-conjuncts)
                                   (maximum-variables default-maximum-variables)
                                   (maximum-spcial-conjuncts default-maximum-spcial-conjuncts)
                                   (maximum-cnjexp-variables default-maximum-cnjexp-variables)
                                   (enable-type default-enable-type)
                                   (enable-glob default-enable-glob)
                                   (ignore-variables default-ignore-variables))
  ;; Load shallow specialization and associate to pm-rbs
  (configure-shallow-specialization-rules pm-rbs
                                          maximum-variables
                                          maximum-spcial-conjuncts
                                          enable-type
                                          enable-glob
                                          ignore-variables)

  ;; Load conjunction-expansion and associate to pm-rbs
  (configure-conjunction-expansion-rules pm-rbs
                                         conjunction-expansion
                                         enforce-specialization
                                         maximum-conjuncts
                                         (min maximum-variables maximum-cnjexp-variables)))

(define* (configure-rules pm-rbs
                          #:key
                          (conjunction-expansion default-conjunction-expansion)
                          (enforce-specialization default-enforce-specialization)
                          (maximum-conjuncts default-maximum-conjuncts)
                          (maximum-variables default-maximum-variables)
                          (maximum-spcial-conjuncts default-maximum-spcial-conjuncts)
                          (maximum-cnjexp-variables default-maximum-cnjexp-variables)
                          (enable-type default-enable-type)
                          (enable-glob default-enable-glob)
                          (ignore-variables default-ignore-variables))
  (configure-mandatory-rules pm-rbs)
  (configure-optional-rules pm-rbs
                            #:conjunction-expansion conjunction-expansion
                            #:enforce-specialization enforce-specialization
                            #:maximum-conjuncts maximum-conjuncts
                            #:maximum-variables maximum-variables
                            #:maximum-spcial-conjuncts maximum-spcial-conjuncts
                            #:maximum-cnjexp-variables maximum-cnjexp-variables
                            #:enable-type enable-type
                            #:enable-glob enable-glob
                            #:ignore-variables ignore-variables))

(define* (configure-surprisingness surp-rbs mode maximum-conjuncts db-ratio)
  ;; Add surprisingness rules
  (let* ((namify (lambda (i) (string-append (symbol->string mode) "-"
                                            (number->string i)
                                            "ary-rule")))
         (aliasify (lambda (i) (DefinedSchema (namify i))))
         (rule-gen (cond ((or (eq? mode 'isurp-old)
                              (eq? mode 'nisurp-old)
                              (eq? mode 'isurp)
                              (eq? mode 'nisurp))
                          (lambda (i) (gen-i-surprisingness-rule mode i db-ratio)))
                         ((eq? mode 'jsdsurp) gen-jsd-surprisingness-rule)))
         (definify (lambda (i) (DefineLink
                                 (aliasify i)
                                 (rule-gen i))))
         (rulify (lambda (i) (definify i) (aliasify i)))
         (rules (map rulify (cdr (iota-plus-one maximum-conjuncts)))))
    (ure-add-rules surp-rbs rules))

  ;; In case of jsdsurp we also need emp, est and jsd rules
  (if (eq? mode 'jsdsurp)
      (let* ((emp-alias (DefinedSchema "emp-rule"))
             (est-alias (DefinedSchema "est-rule"))
             (jsd-alias (DefinedSchema "jsd-rule"))
             (emp-def (Define emp-alias (gen-emp-rule db-ratio)))
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
                          (jobs default-jobs)
                          (maximum-iterations default-maximum-iterations)
                          (complexity-penalty default-complexity-penalty)
                          (conjunction-expansion default-conjunction-expansion)
                          (enforce-specialization default-enforce-specialization)
                          (maximum-conjuncts default-maximum-conjuncts)
                          (maximum-variables default-maximum-variables)
                          (maximum-spcial-conjuncts default-maximum-spcial-conjuncts)
                          (maximum-cnjexp-variables default-maximum-cnjexp-variables)
                          (enable-type default-enable-type)
                          (enable-glob default-enable-glob)
                          (ignore-variables default-ignore-variables))
"
  Given a Concept node representing a rule based system for the
  pattern miner. Automatically configure it with the appropriate
  rules and parameters.

  Usage: (configure-miner pm-rbs
                          #:jobs jb
                          #:maximum-iterations mi
                          #:complexity-penalty cp
                          #:conjunction-expansion ce
                          #:enforce-specialization es
                          #:maximum-conjuncts mc
                          #:maximum-variables mv
                          #:maximum-spcial-conjuncts mspc
                          #:maximum-cnjexp-variables mcev
                          #:enable-type et
                          #:enable-glob eg
                          #:ignore-variables iv)

  pm-rbs: Concept node of the rule-based system to configure

  jb: [optional, default=1] Number of jobs to run in parallel. Can
      speed up mining. Note that this may alter the results especially
      if conjunction expansion if used as its results depends on the output
      of other mining rules.

  mi: [optional, default=100] Maximum number of iterations allocated.
      If negative then the pattern miner keeps running till all patterns
      have been exhausted (not recommended unless you know what you're doing).

  cp: [optional, default=1] Complexity penalty parameter passed to the forward
      chainer. It controls breadth vs depth search. A high value means more
      breadth. A value of 0 means a equilibrium between breadth and depth.
      A negative value means more depth. Possible range is (-inf, +inf)
      but it's rarely necessary in practice to go outside of [-10, 10].

  ce: [optional, default=#t] Flag controlling whether to use the conjunction
      expansion heuristic rules. It will only expand conjunctions with enough
      support with patterns with enough support.

  es: [optional, default=#t] Flag controlling whether specialization is
      enforced.  Some rules such as conjunction expansion can create
      abstractions due to having more variables, this flag enforces that
      only specializations will be created.

  mc: [optional, default=3] In case ce is set to #t, and thus incremental
      conjunction expansion is enabled, that option allows to limit the number
      of conjuncts to mc. If negative then the number of conjuncts can grow
      unlimited (not recommended unless you know what you're doing). As of
      now mc can not be set above 9 (which should be more than enough).

  mv: [optional, default=3] Maximum number of variables that the resulting
      patterns should contain. As of now mv cannot be set above 9 (which
      should be more than enough).

  mcev: [optional, default=2] Maximum number of variables in patterns produced
        by the conjunction expansion rule.

  et: [optional, default=#f] Flag controlling whether the mined patterns will
      have type constraints in their type declaration.  If so, then for
      instance a variable matching only concept nodes will be type restricted
      to concept node and its subtypes.

  eg: [optional, default=#f] Flag controlling whether the mined patterns will
      have glob nodes.  This is convenient when they are links to match with
      different arities.

  iv: [optional, default=()] List of variables to ignore.  This is used for
      instance in temporal mining, where the temporal variable must be left
      untouched.
"
  ;; Load and associate rules to pm-rbs
  (configure-rules pm-rbs
                   #:conjunction-expansion conjunction-expansion
                   #:enforce-specialization enforce-specialization
                   #:maximum-conjuncts maximum-conjuncts
                   #:maximum-variables maximum-variables
                   #:maximum-spcial-conjuncts maximum-spcial-conjuncts
                   #:maximum-cnjexp-variables maximum-cnjexp-variables
                   #:enable-type enable-type
                   #:enable-glob enable-glob
                   #:ignore-variables ignore-variables)

  ;; Set parameters
  (ure-set-jobs pm-rbs jobs)
  (ure-set-maximum-iterations pm-rbs maximum-iterations)
  (ure-set-complexity-penalty pm-rbs complexity-penalty)

  ;; If there is no conjunction expansion then each rule is
  ;; deterministic, thus no need to retry exhausted sources
  (ure-set-fc-retry-exhausted-sources pm-rbs (and conjunction-expansion
                                                  (< 1 maximum-conjuncts))))

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
    (VariableSet
      (Variable \"$X-1\")
      (Variable \"$X-2\")
      (Variable \"$X-3\"))
    (Present
      (Variable \"$X-1\")
      (Variable \"$X-2\")
      (Variable \"$X-3\")))
"
  (let* ((vars (gen-variables "$X" nconj))
         (var-lst (VariableSet vars))
         (var-conj (Present vars)))
    (Lambda var-lst var-conj)))

(define* (cog-miner . args)
  (display ("The command you are looking for is cog-mine.")))

(define (to-number n)
"
  Take a scheme number or a number node in argument
  and return a scheme number.
"
  (if (number? n) n (cog-number n)))

(define (to-number-node n)
"
  Take a scheme number or a number node in argument
  and return a number node.
"
  (if (number? n) (Number n) n))

(define* (cog-mine db
                   #:key
                   ;; Number of jobs to run in parallel
                   (jobs default-jobs)

                   ;; Minimum support
                   (minsup default-minimum-support)
                   (minimum-support default-minimum-support)

                   ;; Minimum frequency
                   (minfreq default-minimum-frequency)
                   (minimum-frequency default-minimum-frequency)

                   ;; Initial pattern
                   (initpat default-initial-pattern)
                   (initial-pattern default-initial-pattern)

                   ;; Maximum number of iterations
                   (maxiter default-maximum-iterations)
                   (maximum-iterations default-maximum-iterations)

                   ;; Complexity penalty
                   (cpxpnlt default-complexity-penalty)
                   (complexity-penalty default-complexity-penalty)

                   ;; Conjunction expansion
                   (cnjexp default-conjunction-expansion)
                   (conjunction-expansion default-conjunction-expansion)

                   ;; Enforce specialization
                   (enfspec default-enforce-specialization)
                   (enforce-specialization default-enforce-specialization)

                   ;; Maximum number of conjunctions
                   (maxcnj default-maximum-conjuncts)
                   (max-conjuncts default-maximum-conjuncts)
                   (maximum-conjuncts default-maximum-conjuncts)

                   ;; Maximum number of variables
                   (maxvar default-maximum-variables)
                   (max-variables default-maximum-variables)
                   (maximum-variables default-maximum-variables)

                   ;; Maximum number of conjuncts specialization can be applied to
                   (maxspcjn default-maximum-spcial-conjuncts)
                   (max-spcial-conjuncts default-maximum-spcial-conjuncts)
                   (maximum-spcial-conjuncts default-maximum-spcial-conjuncts)

                   ;; Maximum number of variables in conjunction expansion
                   (maxcevar default-maximum-cnjexp-variables)
                   (max-cnjexp-variables default-maximum-cnjexp-variables)
                   (maximum-cnjexp-variables default-maximum-cnjexp-variables)

                   ;; Surprisingness measure
                   (surp default-surprisingness)
                   (surprisingness default-surprisingness)

                   ;; db-ratio
                   (db-ratio default-db-ratio)

                   ;; Enable type
                   (enable-type default-enable-type)

                   ;; Enable glob
                   (enable-glob default-enable-glob)

                   ;; Variables to leave untouched
                   (ignore-variables default-ignore-variables))
"
  Mine patterns in db (data trees, a.k.a. grounded hypergraphs) with minimum
  support ms, optionally using mi iterations and starting from the initial
  pattern initpat.

  Usage: (cog-mine db
                   #:jobs jb
                   #:minimum-support ms             (or #:minsup ms)
                   #:minimum-frequency mf           (or #:minfreq mf)
                   #:initial-pattern ip             (or #:initpat ip)
                   #:maximum-iterations mi          (or #:maxiter mi)
                   #:complexity-penalty cp          (or #:cpxpnlt cp)
                   #:conjunction-expansion ce       (or #:cnjexp ce)
                   #:enforce-specialization es      (or #:enfspec es)
                   #:maximum-conjuncts mc           (or #:maxcnj mc)
                   #:maximum-variables mv           (or #:maxvar mv)
                   #:maximum-spcial-conjuncts mspc  (or #:maxspcjn mspc)
                   #:maximum-cnjexp-variables mcev  (or #:maxcevar mcev)
                   #:surprisingness su              (or #:surp su)
                   #:db-ratio dbr
                   #:enable-type et
                   #:enable-glob eg
                   #:ignore-variables iv)

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

  jb: [optional, default=1] Number of jobs to run in parallel. Can
      speed up mining. Note that this may alter the results especially
      if conjunction expansion if used as its results depends on the output
      of other mining rules.

  ms: [optional, default=10] Minimum support. All patterns with count below
      ms are discarded. Can be a Scheme number or an Atomese number node.

  mf: [optional, default=-1] Minimum frequency. All patterns with
      frequency below mf are discarded. Can be a Scheme number or an Atomese
      number node. A normal value ranges from 0 to 1. A negative value means
      disabled, in which case minsup is used instead. Note that the results
      are still described in minsup terms, that this option does is calculate
      the minsup corresponding to a given frequency, such that minsup is equal
      to minfreq * |db|. Can be a Scheme number or an Atomese number node.

  ip: [optional, default=(top)] Initial pattern to start the search from.
      All mined patterns will be specializations of this pattern.

  mi: [optional, default=100] Maximum number of iterations allocated.
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

  mspc: [optional, default=1] Maximum number of conjuncts of a pattern
        to apply shallow specialization to, which can be very expensive
        when applied to patterns with more than 1 conjunct.
        Any value between 0 and the number of conjuncts of the initial
        pattern (excluded) will disable shallow specialization. So make
        sure to properly adjust that parameter if you provide an initial
        pattern (unless you which to disable shallow specialization).

  mcev: [optional, default=2] Maximum number of variables in patterns produced
        by the conjunction expansion rule.

  su: [optional, default='isurp] After running the pattern miner,
      patterns can be ranked according to a surprisingness measure.
      The following supported modes are:

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

      If a surprisingness measure is selected then the pattern miner returns
      a (scheme) list of patterns each wrapped in an evaluation link with
      a predicate indicating their surprisingness. Otherwise, if 'none
      is selected then it returns a (scheme) list of patterns.

  dbr: [optional, default=1] parameter to control how much
       downsampling is taking place to estimate the empirical probability of
       the patterns during surprisingness measure. The surprisingness rules
       may automatically downsample of the dataset to reduce the computational
       cost while maintaining a decent accuracy of the empirical
       probability. However sometimes the user may want to tune that, either
       by downsampling more so save computational resources, or downsampling
       less to improve accuracy. This parameter indicates the proportion of
       the dataset to consider when comparing the estimate of the pattern
       count to the dataset size. The default is 1.0. The rational is that if
       the computer has enough RAM to hold a dataset of size s, then it
       likely has enough RAM to hold s instances of a pattern applied over
       that dataset, thus if the expected count is below s no subsampling is
       taking place, otherwise enough subsampling is taking place as to not
       exceed s. The db-ratio parameter modifies that threshold by
       considering dbr * s instead of s.

       To sum-up:

         dbr < 1 is more efficient but less accurate
         dbr > 1 is less efficient but more accurate

         with dbr ranging from 0 excluded to +inf (also excluded,
         unless you have a super-Turing computer).

       Also, note that such downsampling does not affect the frequent
       mining process, even if dbr is set low, given enough iteration, no
       pattern will be missed, however their surprisingness measures might be
       inaccurate.

  et: [optional, default=#f] Flag controlling whether the mined patterns will
      have type constraints in their type declaration.  If so, then for
      instance a variable matching only concept nodes will be type restricted
      to concept node and its subtypes.

  eg: [optional, default=#f] Flag controlling whether the mined patterns will
      have glob nodes.  This is convenient when they are links to match with
      different arities.

  iv: [optional, default=()] List of variables to ignore.  This is used for
      instance in temporal mining, where the temporal variable must be left
      untouched.

  Under the hood it will create a rule base and a query for the rule
  engine, configure it according to the user's options and run it.
  Everything takes place in a child atomspace. After the job is done
  it will remove the child atomspace after copying the solution set
  in the parent atomspace.

  Pattern mining is computationally demanding. There are a few ways
  to improve performances though.

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
     enable incremental conjunction expansion (the default), see the
     #:conjunction-expansion option.

  5. Enforcing systematic specialization (the default), especially
     when conjunction expansion is used, can dramatically prune the search
     space, however some patterns, like transitivity, will be missed.

  6. If surprisingness takes too low and too much RAM, lower the db-ratio.
"
  (define (diff? x y) (not (equal? x y)))
  (define (num-diff? x y) (not (= (to-number x) (to-number y))))

  ;; Set mininum frequency
  (define mf
    (to-number
     (cond ((num-diff? minimum-frequency default-minimum-frequency) minimum-frequency)
           ((num-diff? minfreq default-minimum-frequency) minfreq)
           (else default-minimum-frequency))))

  ;; Set minimum support
  (define (get-minimum-support db-size)
    (if (<= 0 mf)
        (ceiling (* mf db-size))
        (to-number
         (cond ((num-diff? minimum-support default-minimum-support) minimum-support)
               ((num-diff? minsup default-minimum-support) minsup)
               (else default-minimum-support)))))

  ;; Set initial pattern, a function to use the current atomspace at
  ;; the time of being called
  (define (get-initial-pattern)

    ;; TODO: can be simplified with high order function
    (define (atom-as-readonly? A)
      (define current-as (cog-set-atomspace! (cog-as A)))
      (let* ([A-as-readonly? (cog-atomspace-readonly? A)])
        (cog-set-atomspace! current-as)
        A-as-readonly?))

    ;; TODO: can be simplified with high order function
    (define (atom-as-ro! A)
      (define current-as (cog-set-atomspace! (cog-as A)))
      (let* ([A-as-ro! (cog-atomspace-ro!)])
        (cog-set-atomspace! current-as)
        A-as-ro!))

    ;; TODO: can be simplified with high order function
    (define (atom-as-rw! A)
      (define current-as (cog-set-atomspace! (cog-as A)))
      (let* ([A-as-rw! (cog-atomspace-rw!)])
        (cog-set-atomspace! current-as)
        A-as-rw!))

    (define (copy-to-current-as ip)
      (if (equal? (cog-as ip) (cog-atomspace))
          ;; ip is in current atomspace already, nothing to do
          ip
          ;; ip is not in current atomspace, below is a trick to copy
          ;; it to any atomspace including child atomspace
          (let* ([ip-tv (cog-tv ip)]
                 [ip-as (cog-as ip)])
            (if (atom-as-readonly? ip)
                (cog-set-tv! ip ip-tv)
                ;; Temporary set it to readonly
                (let* ([dummy (atom-as-ro! ip)]
                       [current-as-ip (cog-set-tv! ip ip-tv)])
                  (atom-as-rw! ip)
                  current-as-ip)))))

    (define (add-default-vardecl ip)
      (if (< 1 (cog-arity ip))
          ;; The variable declaration is already there
          ip
          ;; Need to add a default variable declaration
          (let* ((body (cog-outgoing-atom ip 0)))
            (Lambda (VariableSet (cog-free-variables body)) body))))

    (let* ((ip (cond ((diff? initial-pattern default-initial-pattern) initial-pattern)
                     ((diff? initpat default-initial-pattern) initpat)
                     (else default-initial-pattern))))
      ;; If the initial pattern is missing a variable declaration, add
      ;; a default one, as the miner rules require one.
      (add-default-vardecl (copy-to-current-as ip))))

  ;; Set maximum iterations
  (define mi
    (to-number
     (cond ((num-diff? maximum-iterations default-maximum-iterations) maximum-iterations)
           ((num-diff? maxiter default-maximum-iterations) maxiter)
           (else default-maximum-iterations))))

  ;; Set complexity penalty
  (define cp
    (to-number
     (cond ((num-diff? complexity-penalty default-complexity-penalty) complexity-penalty)
           ((num-diff? cpxpnlt default-complexity-penalty) cpxpnlt)
           (else default-complexity-penalty))))

  ;; Set conjunction expansion
  (define ce
    (cond ((diff? conjunction-expansion default-conjunction-expansion) conjunction-expansion)
          ((diff? cnjexp default-conjunction-expansion) cnjexp)
          (else default-conjunction-expansion)))

  ;; Set enforce specialization
  (define es
    (cond ((diff? enforce-specialization default-enforce-specialization) enforce-specialization)
          ((diff? enfspec default-enforce-specialization) enfspec)
          (else default-enforce-specialization)))

  ;; Set maximum conjunctions
  (define mc
    (cond ((diff? maximum-conjuncts default-maximum-conjuncts) maximum-conjuncts)
          ((diff? max-conjuncts default-maximum-conjuncts) max-conjuncts)
          ((diff? maxcnj default-maximum-conjuncts) maxcnj)
          (else default-maximum-conjuncts)))

  ;; Set maximum variables
  (define mv
    (cond ((diff? maximum-variables default-maximum-variables) maximum-variables)
          ((diff? max-variables default-maximum-variables) max-variables)
          ((diff? maxvar default-maximum-variables) maxvar)
          (else default-maximum-variables)))

  ;; Set maximum number of conjuncts specialization can be applied to
  (define mspc
    (cond ((diff? maximum-spcial-conjuncts default-maximum-spcial-conjuncts) maximum-spcial-conjuncts)
          ((diff? max-spcial-conjuncts default-maximum-spcial-conjuncts) max-spcial-conjuncts)
          ((diff? maxspcjn default-maximum-spcial-conjuncts) maxspcjn)
          (else default-maximum-spcial-conjuncts)))

  ;; Set maximum variables in conjunction expansion
  (define mcev
    (cond ((diff? maximum-cnjexp-variables default-maximum-cnjexp-variables) maximum-cnjexp-variables)
          ((diff? max-cnjexp-variables default-maximum-cnjexp-variables) max-cnjexp-variables)
          ((diff? maxcevar default-maximum-cnjexp-variables) maxcevar)
          (else default-maximum-cnjexp-variables)))

  ;; Set surprisingness
  (define su
    (cond ((diff? surprisingness default-surprisingness) surprisingness)
          ((diff? surp default-surprisingness) surp)
          (else default-surprisingness)))

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
         (db-size (get-cardinality db-cpt))
         (ms (get-minimum-support db-size))
         (ms-n (to-number-node ms))
         ;; Check that the initial pattern has enough support
         (es (cog-enough-support? (get-initial-pattern) db-cpt ms-n)))
    (if (not es)
        ;; The initial pattern doesn't have enough support, thus the
        ;; solution set is empty.
        (begin (cog-set-atomspace! parent-as)
               (miner-logger-debug "Initial pattern:\n~a" (get-initial-pattern))
               (miner-logger-debug "Does not have enough support (min support = ~a)" ms)
               (miner-logger-debug "Abort pattern mining")
               ;; TODO: delete tmp-as
               (list))

        ;; The initial pattern has enough support, let's configure the
        ;; rule engine and run the pattern mining query
        (let* (;; Configure pattern miner forward chainer
               (source (minsup-eval-true (get-initial-pattern) db-cpt ms-n))
               (miner-rbs (random-miner-rbs-cpt))
               (cfg-m (configure-miner miner-rbs
                                       #:jobs jobs
                                       #:maximum-iterations mi
                                       #:complexity-penalty cp
                                       #:conjunction-expansion ce
                                       #:enforce-specialization es
                                       #:maximum-conjuncts mc
                                       #:maximum-variables mv
                                       #:maximum-spcial-conjuncts mspc
                                       #:maximum-cnjexp-variables mcev
                                       #:enable-type enable-type
                                       #:enable-glob enable-glob
                                       #:ignore-variables ignore-variables))

               (dummy (miner-logger-debug "Initial pattern:\n~a" (get-initial-pattern)))
               (dummy (miner-logger-debug "Has enough support (min support = ~a)" ms))
               (dummy (miner-logger-debug "Launch URE-based pattern mining"))

               ;; Run pattern miner in a forward way
               (results (cog-fc miner-rbs source))
               ;; Fetch all relevant results
               (patterns (fetch-patterns db-cpt ms-n))
               (patterns-lst (cog-outgoing-set patterns)))

          (if (equal? su 'none)

              ;; No surprisingness, simple return the pattern list
              (let* ((parent-patterns-lst (cog-cp parent-as patterns-lst)))
                (miner-logger-debug "No surprisingness measure, end pattern miner now")
                (cog-set-atomspace! parent-as)
                parent-patterns-lst)

              ;; Run surprisingness
              (let*
                  ;; Configure surprisingness backward chainer
                  ((dummy (miner-logger-debug "Call surprisingness on mined patterns"))

                   (surp-rbs (random-surprisingness-rbs-cpt))
                   (target (surp-target su db-cpt))
                   (vardecl (surp-vardecl))
                   (cfg-s (configure-surprisingness surp-rbs su mc db-ratio))

                   ;; Run surprisingness in a backward way
                   (surp-res (cog-bc surp-rbs target #:vardecl vardecl))
                   (surp-res-lst (cog-outgoing-set surp-res))
                   (surp-res-sort-lst (desc-sort-by-tv-strength surp-res-lst))

                   ;; Copy the results to the parent atomspace
                   (parent-surp-res (cog-cp parent-as surp-res-sort-lst)))
                (miner-logger-debug "End pattern miner")
                (cog-set-atomspace! parent-as)
                parent-surp-res))))))

;;;;;;;;;;;;;;;;;;
;; Miner Logger ;;
;;;;;;;;;;;;;;;;;;

(define (miner-logger-set-level! l)
  "
    Wrapper around cog-logger-set-level! using (cog-miner-logger) as logger.

    See (help cog-logger-set-level!) for more info.
  "
  (cog-logger-set-level! (cog-miner-logger) l))

(define (miner-logger-get-level)
  "
    Wrapper around cog-logger-set-level! using (cog-miner-logger) as logger.

    See (help cog-logger-set-level!) for more info.
  "
  (cog-logger-get-level (cog-miner-logger)))

(define (miner-logger-set-filename! filename)
  "
    Wrapper around cog-logger-set-filename! using (cog-miner-logger) as logger.

    See (help cog-logger-set-filename!) for more info.
  "
  (cog-logger-set-filename! (cog-miner-logger) filename))

(define (miner-logger-get-filename)
  "
    Wrapper around cog-logger-get-filename using (cog-miner-logger) as logger.

    See (help cog-logger-get-filename) for more info.
  "
  (cog-logger-get-filename (cog-miner-logger)))

(define (miner-logger-set-stdout! enable)
  "
    Wrapper around cog-logger-set-stdout! using (cog-miner-logger) as logger.

    See (help cog-logger-set-stdout!) for more info.
  "
  (cog-logger-set-stdout! (cog-miner-logger) enable))

(define (miner-logger-set-sync! enable)
  "
    Wrapper around cog-logger-set-sync! using (cog-miner-logger) as logger.

    See (help cog-logger-set-sync!) for more info.
  "
  (cog-logger-set-sync! (cog-miner-logger) enable))

(define (miner-logger-set-timestamp! enable)
  "
    Wrapper around cog-logger-set-timestamp! using (cog-miner-logger) as logger.

    See (help cog-logger-set-timestamp!) for more info.
  "
  (cog-logger-set-timestamp! (cog-miner-logger) enable))

(define (miner-logger-error-enabled?)
  "
    Wrapper around cog-logger-error-enabled? using (cog-miner-logger) as logger.

    See (help cog-logger-error-enabled?) for more info.
  "
  (cog-logger-error-enabled? (cog-miner-logger)))

(define (miner-logger-warn-enabled?)
  "
    Wrapper around cog-logger-warn-enabled? using (cog-miner-logger) as logger.

    See (help cog-logger-warn-enabled?) for more info.
  "
  (cog-logger-warn-enabled? (cog-miner-logger)))

(define (miner-logger-info-enabled?)
  "
    Wrapper around cog-logger-info-enabled? using (cog-miner-logger) as logger.

    See (help cog-logger-info-enabled?) for more info.
  "
  (cog-logger-info-enabled? (cog-miner-logger)))

(define (miner-logger-debug-enabled?)
  "
    Wrapper around cog-logger-debug-enabled? using (cog-miner-logger) as logger.

    See (help cog-logger-debug-enabled?) for more info.
  "
  (cog-logger-debug-enabled? (cog-miner-logger)))

(define (miner-logger-fine-enabled?)
  "
    Wrapper around cog-logger-fine-enabled? using (cog-miner-logger) as logger.

    See (help cog-logger-fine-enabled?) for more info.
  "
  (cog-logger-fine-enabled? (cog-miner-logger)))

(define (miner-logger-error . args)
  "
    Wrapper around cog-logger-error using (cog-miner-logger) as logger.

    See (help cog-logger-error) for more info.
  "
  (apply cog-logger-error (cons (cog-miner-logger) args)))

(define (miner-logger-warn . args)
  "
    Wrapper around cog-logger-warn using (cog-miner-logger) as logger.

    See (help cog-logger-warn) for more info.
  "
  (apply cog-logger-warn (cons (cog-miner-logger) args)))

(define (miner-logger-info . args)
  "
    Wrapper around cog-logger-info using (cog-miner-logger) as logger.

    See (help cog-logger-info) for more info.
  "
  (apply cog-logger-info (cons (cog-miner-logger) args)))

(define (miner-logger-debug . args)
  "
    Wrapper around cog-logger-debug using (cog-miner-logger) as logger.

    See (help cog-logger-debug) for more info.
  "
  (apply cog-logger-debug (cons (cog-miner-logger) args)))

(define (miner-logger-fine . args)
  "
    Wrapper around cog-logger-fine using (cog-miner-logger) as logger.

    See (help cog-logger-fine) for more info.
  "
  (apply cog-logger-fine (cons (cog-miner-logger) args)))

(define (miner-logger-flush)
  "
    Wrapper around cog-logger-flush using (cog-miner-logger) as logger.

    See (help cog-logger-flush) for more info.
  "
  (cog-logger-flush (cog-miner-logger)))

(define (export-miner-utils)
  (export
    absolutely-true
    iota-plus-one
    top
    random-db-cpt
    random-miner-rbs-cpt
    random-surprisingness-rbs-cpt
    get-db-lst
    fill-db-cpt
    configure-mandatory-rules
    configure-optional-rules
    configure-rules
    configure-surprisingness
    surp-target
    surp-vardecl
    configure-miner
    minsup-eval
    minsup-eval-true
    surp-eval
    get-members
    get-cardinality
    fetch-patterns
    conjunct-pattern
    cog-miner-logger
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
    miner-logger-set-level!
    miner-logger-get-level
    miner-logger-set-filename!
    miner-logger-get-filename
    miner-logger-set-stdout!
    miner-logger-set-sync!
    miner-logger-set-timestamp!
    miner-logger-error-enabled?
    miner-logger-warn-enabled?
    miner-logger-info-enabled?
    miner-logger-debug-enabled?
    miner-logger-fine-enabled?
    miner-logger-error
    miner-logger-warn
    miner-logger-info
    miner-logger-debug
    miner-logger-fine
    miner-logger-flush
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
