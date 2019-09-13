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
;; (cog-logger-set-level! "debug")
;; (cog-logger-set-sync! #t)

;; Set URE logger
(ure-logger-set-level! "debug")
;; (ure-logger-set-timestamp! #f)
;; (ure-logger-set-sync! #t)

;; Set random seed
(cog-randgen-set-seed! 0)

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

  (let* (;; Construct corpus to mine. We select all root atoms except
         ;; quanfiers (ForAll, Exists, ImplicationScope, etc)
         ;; statements, as they usually represent rules rather than
         ;; data.
         (scope? (lambda (x) (cog-subtype? 'ScopeLink (cog-type x))))
         (db
          (filter (lambda (x) (not (scope? x))) (cog-get-all-roots)))
          ;; (filter scope? (cog-get-all-roots)))
          ;; (cog-get-all-roots))

         ;; Build db concept
         (db-cpt (fill-db-cpt (Concept "sumo-db") db))

         ;; Run pattern miner
         (msg-1 (cog-logger-info "Run pattern miner over ~a" kb))
         ;; (results '())
         (results (cog-mine db-cpt
                            #:minfreq mf
                            #:maximum-iterations mi
                            #:conjunction-expansion #t
                            #:max-conjuncts mc
                            #:max-variables mv
                            #:surprisingness 'nisurp))
         (msg-2 (cog-logger-info "Results from mining ~a:\n~a" kb results)))

    ;; We do not return the results because the atomspace is gonna be
    ;; cleared in the next call of that function. Instead the user
    ;; should collect the patterns in the opencog.log file.
    *unspecified*)
)

;; Run the pattern miner over a list of files
(for-each (lambda (args) (apply run-mozi-ai-miner args))
          (list
           (list "kbs/reactome.scm" 0.01 50 2 2)
           (list "kbs/ChEBI2Reactome_PE_Pathway.txt.scm" 0.01 30 2 2)
          )
)
