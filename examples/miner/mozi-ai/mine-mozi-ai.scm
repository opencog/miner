;; Make sure you disable compiling
;;
;; guile --no-auto-compile -l mine-mozi-ai.scm

;; Load modules
(use-modules (opencog randgen))
(use-modules (opencog logger))
(use-modules (opencog ure))
(use-modules (opencog miner))
(use-modules (opencog bioscience))

;; Set main logger
;; (cog-logger-set-timestamp! #f)
(cog-logger-set-level! "debug")
;; (cog-logger-set-sync! #t)

;; Set URE logger
(ure-logger-set-level! "debug")
;; (ure-logger-set-timestamp! #f)
;; (ure-logger-set-sync! #t)

;; Set random seed
(cog-randgen-set-seed! 0)

;; Helpers
(define (scope? x) (cog-subtype? 'ScopeLink (cog-type x)))
(define (lst? x) (cog-subtype? 'ListLink (cog-type x)))
(define (eval-pred-name? name x) (and (cog-subtype? 'EvaluationLink (cog-type x))
                                      (equal? (cog-name (gar x)) name)))

;; Function to run the pattern miner on a given file with the follow
;; parameters
;;
;; mf: minimum frequency
;; mi: maximum number of iterations
;; mc: maximum number of conjuncts
;; mv: maximum number of variables
(define (run-mozi-ai-miner kb mf mi mc mv)
  (clear)
  (load kb)

  (let* (;; Construct corpus to mine.
         (db (cog-atomspace))
         (db-lst (get-db-lst db))

         ;; Filter out types from db-lst
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
         (db-filtered-lst (filter admissible? db-lst))

         (msg-0 (cog-logger-debug "db (size = ~a):\n~a" (length db-filtered-lst) db-filtered-lst))

         ;; Build db concept
         (db-cpt (fill-db-cpt (Concept "sumo-db") db-filtered-lst))

         ;; Run pattern miner
         (msg-1 (cog-logger-info "Run pattern miner over ~a" kb))
         (msg-2 (cog-logger-info (string-append "With parameters:\n"
                                                "minfreq = ~a\n"
                                                "max-iterations = ~a\n"
                                                "max-conjuncts = ~a\n"
                                                "max-variables = ~a")
                                 mf mi mc mv))
         ;; (results '())
         (results (cog-mine db-cpt
                            #:minfreq mf
                            #:maximum-iterations mi
                            #:conjunction-expansion #t
                            #:max-conjuncts mc
                            #:max-variables mv
                            #:surprisingness 'nisurp))
         (msg-3 (cog-logger-info "Results from mining ~a:\n~a" kb results)))

    ;; We do not return the results because the atomspace is gonna be
    ;; cleared in the next call of that function. Instead the user
    ;; should collect the patterns in the opencog.log file.
    *unspecified*)
)

;; Run the pattern miner over a list of files
(for-each (lambda (args) (apply run-mozi-ai-miner args))
          (list
           (list "kbs/bestLMPDmoses.scm" 0.01 100 2 2)
           ;; (list "kbs/all.scm" 0.001 100 2 2)
           ;; (list "kbs/reactome.scm" 0.01 50 2 2)
           ;; (list "kbs/ChEBI2Reactome_PE_Pathway.txt.scm" 0.01 30 2 2)
          )
)
