;; Script to calculate the surprisingness of a given collection of
;; patterns over a given kb. This is convenient to test if the
;; surprisingness of a given pattern holds across multiple kbs.
;;
;; Make sure you disable compiling
;;
;; guile --no-auto-compile -l surp-mozi-ai.scm

;; Parameters
(define kb-filename "all-pp.scm")
(define ptns-filename "test.scm")
(define surp 'nisurp)
(define max-cnjs 3)
(define db-ratio 1)

;; Load modules & utils
(use-modules (srfi srfi-1))
(use-modules (opencog logger))
(use-modules (opencog ure))
(use-modules (opencog miner))
(use-modules (opencog bioscience))
(load "mozi-ai-utils.scm")

;; Set loggers
(define log-filename (string-append
                      (rm-extension ptns-filename "scm")
                      "-"
                      (rm-extension kb-filename "scm")
                      "-" (symbol->string surp)
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

;; Function to load the patterns and calculate their
;; surprisingness. Patterns can be already wrapped with a
;; surprisingness measure. In such case such function will replace it.
(define (run-surprisingness)
  (let* (;; db concept
         (db-cpt (Concept kb-filename))

         ;; Load patterns
         (ptns-filepath (string-append "ptns/" ptns-filename))
         (msg (cog-logger-debug "Loading ~a" ptns-filepath))
         (ptns-lst (load-patterns ptns-filepath db-cpt))

         ;; Load kb
         (kb-filepath (string-append "kbs/" kb-filename))
         (msg (cog-logger-debug "Loading ~a" kb-filepath))
         (db-lst (load-min-preprocess kb-filepath))

         ;; Build db concept
         (dmy (fill-db-cpt db-cpt db-lst))

         ;; Surprisingness
         (msg (cog-logger-info "Run surprisingness over ~a with patterns ~a"
                               kb-filename ptns-filename))
         (msg (cog-logger-info (string-append "With parameters:\n"
                                              "surp = ~a\n"
                                              "max-cnjs = ~a")
                               surp max-cnjs))
         (results (cog-outgoing-set (cog-surp surp max-cnjs db-cpt db-ratio)))
         (msg (cog-logger-info "Results of surprisingness over ~a:\nsize = ~a\n~a"
                               kb-filename (length results) results)))
    results))

;; Run surprisingness
(define results (run-surprisingness))
