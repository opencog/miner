ADD_GUILE_EXTENSION(SCM_CONFIG guile-miner "opencog-ext-path-miner")

ADD_GUILE_MODULE (FILES
  miner.scm
  miner/miner-utils.scm
  miner/rules/miner-rule-utils.scm
  miner/rules/shallow-abstraction.scm
  miner/rules/specialization.scm
  miner/rules/shallow-specialization.scm
  miner/rules/conjunction-expansion.scm
  miner/rules/i-surprisingness.scm
  miner/rules/emp.scm
  miner/rules/est.scm
  miner/rules/jsd.scm
  miner/rules/jsd-surprisingness.scm
  MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/miner"
)
