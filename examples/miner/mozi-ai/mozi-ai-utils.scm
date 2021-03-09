;; Set of utilities for filtering the dataset and the results

;; Load modules
(use-modules (srfi srfi-1))
(use-modules (opencog miner))
(use-modules (opencog logger))

;; Helpers
(define (scope? x)
  (cog-subtype? 'ScopeLink (cog-type x)))

(define (lst? x)
  (cog-subtype? 'ListLink (cog-type x)))

(define (and? x)
  (cog-subtype? 'AndLink (cog-type x)))

(define (eval? x)
  (cog-subtype? 'EvaluationLink (cog-type x)))

(define (eval-pred-name? name x)
  (and (eval? x)
       (equal? (cog-name (gar x)) name)))

(define (forall? p l)
  (if (null? l)
      #t
      (if (p (car l))
          (forall? p (cdr l))
          #f)))

(define (exist? p l)
  (if (null? l)
      #f
      (if (p (car l))
          #t
          (exist? p (cdr l)))))

(define (subgraph? x y)
  (if (equal? x y)
      #t
      (if (cog-link? y)
          (exist? (lambda (s) (subgraph? x s)) (cog-outgoing-set y))
          #f)))

(define (member? x lst)
  (if (member x lst) #t #f))

(define (delete-subgraph-not-in x lst)
  (if (and (cog-atom? x) (not (member? x lst)))
      (let* ((outgoings (cog-outgoing-set x))
             (del (lambda (x) (delete-subgraph-not-in x lst))))
        (cog-logger-debug "cog-extract! ~a" x)
        (cog-extract! x)
        (map del outgoings))))

(define (get-pattern se)
"
 Given an surprisingness evaluation like

 (Evaluation (Predicate \"nisurp\") <pattern>)

 return <pattern>
"
  (cog-outgoing-atom (gdr se) 0))

(define (get-body pattern)
"
 Given a pattern like

 (Lambda <vardecl> <body>)

 return <body>
"
  (cog-outgoing-atom pattern 1))

(define (get-eval-args eval)
"
  Given an evaluation link like

  (Evaluation <predicate> (List <arg1> ... <argn>))

  return (<arg1> ... <argn>)
"
  (let* ((args (gdr eval)))
    (if (lst? args)
        (cog-outgoing-set args)
        args)))

(define (exist-pair? p l)
"
  Given a property p and list l, find a pair in that list such that p is true.
  p is assumed to be symmetric and irreflexive.
"
  (if (= (length l) 2)
      ;; Base case
      (p (car l) (cadr l))
      ;; Recursive case
      (exist? (lambda (x) (p (car l) x)) (cdr l))))

(define (load-preprocess kb-filename)
"
  Load the given dataset and remove useless atoms for mining.
"
  (let* (;; Load the corpus in a seperate atomspace
         (base-as (cog-push-atomspace))
         (dummy (load kb-filename))

         ;; Construct corpus to mine.
         (db-as (cog-atomspace))
         (db-lst (get-db-lst db-as))

         ;; Filter in admissible types from db-lst
         (eval-has_pubmedID? (lambda (x) (eval-pred-name? "has_pubmedID" x)))
         (eval-has_definition? (lambda (x) (eval-pred-name? "has_definition" x)))
         (eval-has_name? (lambda (x) (eval-pred-name? "has_name" x)))
         (eval-GO_definition? (lambda (x) (eval-pred-name? "GO_definition" x)))
         (eval-GO_name? (lambda (x) (eval-pred-name? "GO_name" x)))
         (admissible? (lambda (x) (and
                                    (cog-link? x)
                                    (not (scope? x))
                                    (not (lst? x))
                                    (not (and? x))
                                    (not (eval-has_pubmedID? x))
                                    (not (eval-has_definition? x))
                                    (not (eval-has_name? x))
                                    (not (eval-GO_definition? x))
                                    (not (eval-GO_name? x)))))
         (db-in-lst (filter admissible? db-lst))

         ;; ;; TMP log pre-processed DB
         ;; (msg (cog-logger-debug "db-in-lst:\n~a" db-in-lst))

         ;; Copy admissible atoms in the base atomspace
         (base-db-in-lst (cog-cp base-as db-in-lst))

         ;; Discard the db atomspace
         (dummy (cog-pop-atomspace)))
    base-db-in-lst))

(define (load-min-preprocess kb-filename)
"
  Load the given dataset, but do minimal filtering (only retain links).
"
  (let* (;; Load the corpus in a seperate atomspace
         (base-as (cog-push-atomspace))
         (dummy (load kb-filename))

         ;; Construct corpus to mine.
         (db-as (cog-atomspace))
         (db-lst (get-db-lst db-as))

         ;; Filter in addimissible types from db-lst
         (admissible? (lambda (x) (and
                                    (cog-link? x)
                                    (not (scope? x))
                                    (not (lst? x))
                                    (not (and? x)))))
         (db-in-lst (filter admissible? db-lst))

         ;; ;; TMP log pre-processed DB
         ;; (msg (cog-logger-debug "db-in-lst:\n~a" db-in-lst))

         ;; Copy admissible atoms in the base atomspace
         (base-db-in-lst (cog-cp base-as db-in-lst))

         ;; Discard the db atomspace
         (dummy (cog-pop-atomspace)))
    base-db-in-lst))

(define (postprocess results max-cnjs)
"
 Till reasoning is fully enabled, postprocess the results to remove
 obvious patterns such as (Present P(X, Y) P(Y, X)) when P is symmetric.
"
  (define (reverse? l1 l2)
    (equal? l1 (reverse l2)))
  (define (enough-clauses? pattern)
    (let* ((body (get-body pattern))
           (clauses (cog-outgoing-set body)))
      (= (length clauses) max-cnjs)))
  (define (symmetric? pattern)
    (let* ((body (get-body pattern))
           (clauses (cog-outgoing-set body))
           (eval-interacts-with? (lambda (x) (eval-pred-name? "interacts_with" x)))
           (interacts-with-clauses (filter eval-interacts-with? clauses))
           (reverse-args? (lambda (left-clause right-clause)
                            (let* ((left-args (get-eval-args left-clause))
                                   (right-args (get-eval-args right-clause)))
                              (reverse? left-args right-args))))
           (smtr? (if (<= 2 (length interacts-with-clauses))
                      (exist-pair? reverse-args? interacts-with-clauses)
                      #f)))
      smtr?))
  (define (in? result)
    (let* ((pattern (get-pattern result)))
      (and (enough-clauses? pattern)
           (not (symmetric? (get-pattern result))))))
  (filter in? results))

(define (rm-extension fn ext)
  (if (string-suffix? (string-append "." ext) fn)
      (substring fn 0 (- (string-length fn) 4))
      fn))

(define (load-patterns ptns-filename db-cpt)
  (let* (;; Load the patterns in a seperate atomspace
         (base-as (cog-push-atomspace))
         (dummy (load ptns-filename))

         ;; Construct patterns with the right format (minsup
         ;; evaluations, as the miner will require that to evaluate
         ;; their surprisingness)
         (ptns-as (cog-atomspace))
         (ptns-lst (filter eval? (cog-get-all-roots)))

         ;; Replace whatever evaluation by minsup
         (ms 1)
         (ptn->minsup (lambda (ptn)
                        (minsup-eval-true (get-pattern ptn)
                                                      db-cpt
                                                      ms)))
         (minsup-ptns-lst (map ptn->minsup ptns-lst))

         ;; Copy minsup patterns in the base atomspace
         (base-minsup-ptns-lst (cog-cp base-as minsup-ptns-lst))

         ;; Discard the db atomspace
         (dummy (cog-pop-atomspace)))
    base-minsup-ptns-lst))

;; TODO: move this to miner-utils.scm, maybe
(define (cog-surp surprisingness max-conjuncts db-cpt db-ratio)
  (let* (
         ;; Configure surprisingness backward chainer
         (surp-rbs (random-surprisingness-rbs-cpt))
         (target (surp-target surprisingness db-cpt))
         (vardecl (surp-vardecl))
         (cfg-s (configure-surprisingness surp-rbs
                                          surprisingness
                                          max-conjuncts
					  db-ratio))

         ;; Run surprisingness in a backward way
         (results (cog-bc surp-rbs target #:vardecl vardecl)))
    results))
