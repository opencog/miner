ADD_LIBRARY(miner SHARED
	Miner
	MinerLogger
	MinerUtils
	HandleTree
	Valuations
	Surprisingness
)

TARGET_LINK_LIBRARIES(miner
	${COGUTIL_LIBRARY}
	${ATOMSPACE_LIBRARIES}
	${URE_LIBRARIES}
)

INSTALL (TARGETS miner DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	Miner.h
	MinerLogger.h
	MinerUtils.h
	HandleTree.h
	Valuations.h
	Surprisingness.h
	DESTINATION "include/opencog/miner"
)

IF (HAVE_GUILE)
  ADD_LIBRARY(guile-miner SHARED
    MinerSCM
  )

  TARGET_LINK_LIBRARIES(guile-miner
    miner
    ${GUILE_LIBRARIES}
  )

  INSTALL (TARGETS guile-miner DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

  ADD_GUILE_MODULE (FILES
    miner.scm
    miner-utils.scm
    rules/miner-rule-utils.scm
    rules/shallow-abstraction.scm
    rules/specialization.scm
    rules/shallow-specialization.scm
    rules/conjunction-expansion.scm
    rules/i-surprisingness.scm
    rules/emp.scm
    rules/est.scm
    rules/jsd.scm
    rules/jsd-surprisingness.scm
    MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/miner"
  )
  ADD_GUILE_EXTENSION(SCM_CONFIG guile-miner "opencog-ext-path-miner")

ENDIF (HAVE_GUILE)
