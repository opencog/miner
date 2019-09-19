;; Make sure you disable compiling
;;
;; guile --no-auto-compile -l mine-mozi-ai.scm

;; Load modules
(use-modules (srfi srfi-1))
(use-modules (opencog randgen))
(use-modules (opencog logger))
(use-modules (opencog ure))
(use-modules (opencog miner))
(use-modules (opencog bioscience))

;; Set parameters
(define kb-filename "bestLMPDmoses.scm")
(define min-freq 0.001)
(define max-iter 250)
(define max-cnjs 3)
(define max-vars 2)
(define rand-seed 0)

;; Set random seed
(cog-randgen-set-seed! rand-seed)

;; Set loggers
(define (rm-scm-extension fn)
  (if (string-suffix? ".scm" fn)
      (substring fn 0 (- (string-length fn) 4))
      fn))
(define log-filename (string-append
                      "opencog-"
                      (rm-scm-extension kb-filename)
                      "-s" (number->string rand-seed)
                      "-mf" (number->string min-freq)
                      "-mi" (number->string max-iter)
                      "-mc" (number->string max-cnjs)
                      "-mv" (number->string max-vars)
                      ".log"))

;; Set main logger
;; (cog-logger-set-timestamp! #f)
(cog-logger-set-level! "debug")
;; (cog-logger-set-sync! #t)
(cog-logger-set-filename! log-filename)

;; Set URE logger
(ure-logger-set-level! "debug")
;; (ure-logger-set-timestamp! #f)
;; (ure-logger-set-sync! #t)
(ure-logger-set-filename! log-filename)

;; Helpers
(define (scope? x)
  (cog-subtype? 'ScopeLink (cog-type x)))

(define (lst? x)
  (cog-subtype? 'ListLink (cog-type x)))

(define (eval-pred-name? name x)
  (and (cog-subtype? 'EvaluationLink (cog-type x))
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
        (cog-logger-debug "cog-delete ~a" x)
        (cog-delete x)
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

(define (postprocess results)
"
 Till reasoning is fully enabled, postprocess the results to remove
 obvious patterns such as (Present P(X, Y) P(Y, X)) when P is symmetric.
"
  (define (reverse? l1 l2)
    (equal? l1 (reverse l2)))
  (define (enough-clauses? pattern)
    (let* ((body (get-body pattern))
           (clauses (cog-outgoing-set body)))
      (= (length clauses) max-conjuncts)))
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

;; Function to run the pattern miner on a given file with the follow
;; parameters
;;
;; mf: minimum frequency
;; mi: maximum number of iterations
;; mc: maximum number of conjuncts
;; mv: maximum number of variables
(define (run-mozi-ai-miner kb mf mi mc mv)
  (let* (;; Load the corpus in a seperate atomspace
         (base-as (cog-push-atomspace))
         (dummy (load kb))

         ;; Construct corpus to mine.
         (db-as (cog-atomspace))
         (db-lst (get-db-lst db-as))
         (msg (cog-logger-debug "db-as size = ~a" (count-all)))

         ;; Filter in addimissible types from db-lst
         (eval-has_pubmedID? (lambda (x) (eval-pred-name? "has_pubmedID" x)))
         (eval-has_definition? (lambda (x) (eval-pred-name? "has_definition" x)))
         (eval-has_name? (lambda (x) (eval-pred-name? "has_name" x)))
         (eval-GO_definition? (lambda (x) (eval-pred-name? "GO_definition" x)))
         (admissible? (lambda (x) (and
                                    (cog-link? x)
                                    (not (scope? x))
                                    (not (lst? x))
                                    (not (eval-has_pubmedID? x))
                                    (not (eval-has_definition? x))
                                    (not (eval-has_name? x))
                                    (not (eval-GO_definition? x)))))
         (db-in-lst (filter admissible? db-lst))
         (msg (cog-logger-debug "db-in-lst size = ~a" (length db-in-lst)))

         ;; Copy admissibal atoms in the base atomspace
         (base-db-in-lst (cog-cp base-as db-in-lst))

         ;; Discard the db atomspace
         (dummy (cog-pop-atomspace))
         (msg (cog-logger-debug "base-as size = ~a" (count-all)))
         (msg (cog-logger-fine "base-db-in-lst = ~a" base-db-in-lst))

         ;; Build db concept
         (db-cpt (fill-db-cpt (Concept kb) base-db-in-lst))

         ;; Run pattern miner
         (msg (cog-logger-info "Run pattern miner over ~a" kb))
         (msg (cog-logger-info (string-append "With parameters:\n"
                                              "random-seed = ~a\n"
                                              "min-frequency = ~a\n"
                                              "max-iterations = ~a\n"
                                              "max-conjuncts = ~a\n"
                                              "max-variables = ~a")
                               rand-seed mf mi mc mv))
         (results (cog-mine db-cpt
                            #:minfreq mf
                            #:maximum-iterations mi
                            #:conjunction-expansion #t
                            #:max-conjuncts mc
                            #:max-variables mv
                            #:surprisingness 'nisurp))
         (pp-results (postprocess results))
         (msg (cog-logger-info "Results from mining ~a:\nsize = ~a\n~a"
                               kb (length pp-results) pp-results)))
    pp-results))

;; Run the pattern miner over the kb with the given parameters
(define results (run-mozi-ai-miner (string-append "kbs/" kb-filename)
                                   min-freq max-iter max-cnjs max-vars))
