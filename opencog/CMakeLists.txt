DECLARE_GUILE_CONFIG_TARGET(SCM_CONFIG "opencog miner-config" "MINER_TEST")

IF (HAVE_MINER)
	ADD_SUBDIRECTORY (miner)
ENDIF (HAVE_MINER)

WRITE_GUILE_CONFIG(${GUILE_BIN_DIR}/opencog/miner-config.scm SCM_CONFIG TRUE)

WRITE_GUILE_CONFIG(${GUILE_BIN_DIR}/opencog/miner-config-installable.scm SCM_CONFIG FALSE)
INSTALL(FILES ${GUILE_BIN_DIR}/opencog/miner-config-installable.scm DESTINATION ${GUILE_SITE_DIR}/opencog RENAME miner-config.scm)
