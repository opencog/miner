ADD_LIBRARY(miner SHARED
	Miner
	MinerLogger
	MinerUtils
	HandleTree
	Valuations
	Surprisingness
)

TARGET_LINK_LIBRARIES(miner
	${URE_LIBRARIES}
	${UNIFY_LIBRARIES}
	${ATOMSPACE_LIBRARIES}
	${COGUTIL_LIBRARY}
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

  ADD_SUBDIRECTORY(scm)

  INSTALL(TARGETS guile-miner DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

ENDIF (HAVE_GUILE)
