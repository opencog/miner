;;
;; OpenCog Miner module
;;
(define-module (opencog miner))

(use-modules (opencog miner-config))
;; This loads the miner atom types.
(load-extension (string-append opencog-ext-path-miner "libguile-miner") "opencog_miner_init")

;; Load miner utils

(include-from-path "opencog/miner/miner-utils.scm")
(export-miner-utils)
