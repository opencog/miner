ADD_LIBRARY(miner SHARED
	Miner.cc
	MinerLogger.cc
	MinerUtils.cc
	HandleTree.cc
	Valuations.cc
	Surprisingness.cc
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
		MinerSCM.cc
	)

  TARGET_LINK_LIBRARIES(guile-miner
    miner
    ${GUILE_LIBRARIES}
  )

  ADD_SUBDIRECTORY(scm)

  INSTALL(TARGETS guile-miner DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

ENDIF (HAVE_GUILE)
