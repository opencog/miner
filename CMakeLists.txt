#
# Master Miner CMake file.
#
# General organization:
# -- check for different compilers, OS'es
# -- search for various required & optional libraries/tools
# -- decide what to build based on above results.
# -- configure various config files.
# -- print pretty summary
#

# cogutils already requires 3.12, so may as well ask for that.
CMAKE_MINIMUM_REQUIRED(VERSION 3.12)

PROJECT(miner)

# ----------------------------------------------------------
# User-modifiable options. Feel free to change these!
#
# uncomment to be in Release mode [default]
# SET(CMAKE_BUILD_TYPE Release)

# uncomment to build in debug mode
# SET(CMAKE_BUILD_TYPE Debug)

# uncomment to be in coverage testing mode
# SET(CMAKE_BUILD_TYPE Coverage)

# uncomment to build in profile mode
# SET(CMAKE_BUILD_TYPE Profile)

# uncomment to build in release mode with debug information
# SET(CMAKE_BUILD_TYPE RelWithDebInfo)

# default build type
IF (CMAKE_BUILD_TYPE STREQUAL "")
	SET(CMAKE_BUILD_TYPE Release)
ENDIF (CMAKE_BUILD_TYPE STREQUAL "")

MESSAGE(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

ADD_DEFINITIONS(-DPROJECT_SOURCE_DIR="${CMAKE_SOURCE_DIR}"
                -DPROJECT_BINARY_DIR="${CMAKE_BINARY_DIR}")

# ===============================================================
# Check for existance of various required, optional packages.
# Listed in alphabetical order, more or less.
# CogUtil must come first, because it supplies various FindXXX macros.

# Add the 'lib' dir to cmake's module search path
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/lib/")

# Cogutil
FIND_PACKAGE(CogUtil CONFIG REQUIRED)
IF (COGUTIL_FOUND)
	MESSAGE(STATUS "CogUtil version ${COGUTIL_VERSION} found.")
	ADD_DEFINITIONS(-DHAVE_COGUTIL)
	SET(HAVE_COGUTIL 1)
ELSE (COGUTIL_FOUND)
	MESSAGE(FATAL_ERROR "CogUtil missing: it is needed!")
ENDIF (COGUTIL_FOUND)

# AtomSpace
FIND_PACKAGE(AtomSpace 5.0.3 CONFIG REQUIRED)
IF (ATOMSPACE_FOUND)
	MESSAGE(STATUS "AtomSpace found.")
	ADD_DEFINITIONS(-DHAVE_ATOMSPACE)
	SET(HAVE_ATOMSPACE 1)
ELSE (ATOMSPACE_FOUND)
	MESSAGE(FATAL_ERROR "AtomSpace missing: it is needed!")
ENDIF (ATOMSPACE_FOUND)

# Unify
FIND_PACKAGE(Unify 1.0.0 CONFIG REQUIRED)
IF (UNIFY_FOUND)
	MESSAGE(STATUS "Unify found.")
	ADD_DEFINITIONS(-DHAVE_UNIFY)
	SET(HAVE_UNIFY 1)
ELSE (UNIFY_FOUND)
	MESSAGE(STATUS "Unify missing: it is needed!")
ENDIF (UNIFY_FOUND)

# URE
FIND_PACKAGE(URE 1.0.0 CONFIG REQUIRED)
IF (URE_FOUND)
	MESSAGE(STATUS "URE found.")
	ADD_DEFINITIONS(-DHAVE_URE)
	SET(HAVE_URE 1)
ELSE (URE_FOUND)
	MESSAGE(STATUS "URE missing: it is needed!")
ENDIF (URE_FOUND)

# add the 'cmake' directory from cogutil to search path
list(APPEND CMAKE_MODULE_PATH  ${COGUTIL_DATA_DIR}/cmake)
include(OpenCogGccOptions)
include(OpenCogLibOptions)
include(OpenCogInstallOptions)
include(Summary)

# ----------------------------------------------------------
# Check for boost. We need dynamic-linked, threaded libs by default.
SET(Boost_USE_STATIC_LIBS OFF)
SET(Boost_USE_MULTITHREADED ON)
SET(MIN_BOOST 1.46)

# Required boost packages
FIND_PACKAGE(Boost ${MIN_BOOST} REQUIRED)

IF(Boost_FOUND)
	SET(Boost_FOUND_SAVE 1)
ELSE(Boost_FOUND)
	MESSAGE(FATAL_ERROR "Boost ${MIN_BOOST} or newer is needed to build OpenCog!")
ENDIF(Boost_FOUND)

# Opencog won't compile with Boost 1.51, some kind of conflict with
# hash functions, see github bugs 1 and 36
IF(105100 EQUAL ${Boost_VERSION})
	MESSAGE(FATAL_ERROR "Boost version 1.51 will not work with OpenCog.  Please use a different version.")
ENDIF(105100 EQUAL ${Boost_VERSION})
MESSAGE(STATUS "Boost version ${Boost_VERSION} found.")

# Optional boost packages; can build without these.
FIND_PACKAGE(Boost ${MIN_BOOST} COMPONENTS python program_options QUIET)
FIND_PACKAGE(Boost ${MIN_BOOST} COMPONENTS math_c99 QUIET)

# Arghhh. Except cmake is treating above as required, not optional. #$%**&
IF(Boost_FOUND_SAVE)
	SET(Boost_FOUND 1)
ENDIF(Boost_FOUND_SAVE)

# ----------------------------------------------------------
# Needed for unit tests

FIND_PACKAGE(Cxxtest)
IF (NOT CXXTEST_FOUND)
	MESSAGE(STATUS "CxxTest missing: needed for unit tests.")
ENDIF (NOT CXXTEST_FOUND)

# ----------------------------------------------------------
# This is required for Guile

include(OpenCogFindGuile)

# ----------------------------------------------------------
# Optional, currently needed only to hush up DRD in util/Logger.cc
FIND_PACKAGE(VALGRIND)
IF (VALGRIND_FOUND)
	MESSAGE(STATUS "VALGRIND was found.")
	IF (VALGRIND_INCLUDE_DIR)
		MESSAGE(STATUS "VALGRIND devel headers found.")
		ADD_DEFINITIONS(-DHAVE_VALGRIND)
	ELSE (VALGRIND_INCLUDE_DIR)
		MESSAGE(STATUS "VALGRIND devel headers NOT FOUND: needed for thread debugging.")
	ENDIF (VALGRIND_INCLUDE_DIR)
ELSE (VALGRIND_FOUND)
	MESSAGE(STATUS "VALGRIND missing: needed for thread debugging.")
ENDIF (VALGRIND_FOUND)

# ===================================================================
# Include configuration.

# Set default include paths.
INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR} ${Boost_INCLUDE_DIRS}
	${COGUTIL_INCLUDE_DIR} ${ATOMSPACE_INCLUDE_DIR})

# Macros that define how atom types get declared.
IF (NOT DEFINED ATOMSPACE_DATA_DIR)
	SET (ATOMSPACE_DATA_DIR "${COGUTIL_DATA_DIR}")
ENDIF (NOT DEFINED ATOMSPACE_DATA_DIR)

INCLUDE("${ATOMSPACE_DATA_DIR}/cmake/OpenCogMacros.cmake")
INCLUDE("${ATOMSPACE_DATA_DIR}/cmake/OpenCogGuile.cmake")
INCLUDE("${ATOMSPACE_DATA_DIR}/cmake/OpenCogCython.cmake")

# ==========================================================
# Decide what to build, based on the packages found.

IF(HAVE_URE)
	SET(HAVE_MINER 1)
ENDIF(HAVE_URE)

ADD_SUBDIRECTORY(opencog)

IF (CXXTEST_FOUND)
	ADD_CUSTOM_TARGET(tests)
	ADD_SUBDIRECTORY(tests EXCLUDE_FROM_ALL)
	IF (CMAKE_BUILD_TYPE STREQUAL "Coverage")
		# doing coverage stuff while running tests if this is the Coverage build
		ADD_CUSTOM_TARGET(check
			# TODO lcov should be found by cmake first
			# TODO set it up so that we can pick to run coverage per test, or
			# combined across all tests (the latter is MUCH faster). Use a define?
			# There is coverage specific stuff in AddCxxTest.cmake now...
			# -
			# Depends on cogserver because RESTFulTest needs it
			# and cmake has no way to add non-test dependencies to tests
			DEPENDS tests cogserver
			WORKING_DIRECTORY tests
			COMMAND ${CMAKE_CTEST_COMMAND} --force-new-ctest-process --output-on-failure $(ARGS)

			# This script combines the coverage analysis of each test,
			# then creates html in tests/lcov
			# Note: this should now be run separately...
			#COMMAND ${PROJECT_SOURCE_DIR}/scripts/combine_lcov.sh
			COMMENT "Running tests with coverage..."
		)
	ELSE (CMAKE_BUILD_TYPE STREQUAL "Coverage")
		# If this is a build with coverage enabled then test normally
		ADD_CUSTOM_TARGET(check
			DEPENDS tests
			WORKING_DIRECTORY tests
			COMMAND ${CMAKE_CTEST_COMMAND} --force-new-ctest-process --output-on-failure $(ARGS)
			COMMENT "Running tests..."
		)
	ENDIF (CMAKE_BUILD_TYPE STREQUAL "Coverage")
ENDIF (CXXTEST_FOUND)

ADD_CUSTOM_TARGET(cscope
	COMMAND find opencog examples tests -name '*.cc' -o -name '*.h' -o -name '*.cxxtest' -o -name '*.scm' > ${CMAKE_SOURCE_DIR}/cscope.files
	COMMAND cscope -b
	WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
	COMMENT "Generating CScope database"
)

# ===================================================================
# Packaging
## Architecture the package is for.
## TODO: Will give error on non debian distros, fix it.
EXECUTE_PROCESS(COMMAND  dpkg --print-architecture
	OUTPUT_VARIABLE PACKAGE_ARCHITECTURE
	OUTPUT_STRIP_TRAILING_WHITESPACE)
STRING(TIMESTAMP UTC_DATE %Y%m%d UTC)
# If 'sudo make install' is run before 'make package', then install_manifest.txt
# will be owned by root. Creating the file during configuration stage ensures
# that it is owned by the builder thus avoiding 'Permission denied' error when
# packaging.
FILE(WRITE "${PROJECT_BINARY_DIR}/install_manifest.txt")
## It doesn't have a header-file declaring the version similar to cogutil and
## atomspace.
SET(SEMANTIC_VERSION 1.0.0)

## Cpack configuration
SET(CPACK_GENERATOR "DEB")
SET(CPACK_PACKAGE_CONTACT "opencog@googlegroups.com")
SET(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
SET(CPACK_PACKAGE_DIRECTORY "${CMAKE_BINARY_DIR}/packages")
SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "OpenCog Pattern Miner")
SET(CPACK_PACKAGE_NAME "miner-dev")
SET(CPACK_PACKAGE_VENDOR "opencog.org")
SET(CPACK_PACKAGE_VERSION "${SEMANTIC_VERSION}-${UTC_DATE}")
SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
SET(CPACK_PACKAGE_FILE_NAME
	"${CPACK_PACKAGE_NAME}_${CPACK_PACKAGE_VERSION}_${PACKAGE_ARCHITECTURE}")
SET(CPACK_PACKAGING_INSTALL_PREFIX "/usr/local")

## Debian specific configurations
SET(DEPENDENCY_LIST
	"guile-2.2-dev (>= 2.2.2)"
	"python-dev (>= 2.7.5)"
	"libboost-random-dev (>= ${MIN_BOOST})"
	"libstdc++6 (>= 4.7)"
	"libcogutil-dev (>= 2.0.2)"
	"atomspace-dev (>= 5.0.3)"
)

STRING(REPLACE ";" ", " MAIN_DEPENDENCIES "${DEPENDENCY_LIST}")
SET(CPACK_DEBIAN_PACKAGE_DEPENDS "${MAIN_DEPENDENCIES}")
SET(CPACK_DEBIAN_PACKAGE_SECTION "libdevel")
SET(CPACK_DEBIAN_PACKAGE_HOMEPAGE "http://opencog.org")
INCLUDE(CPack)

# ===================================================================
# Show a summary of what we found, what we will do.

SUMMARY_ADD("Miner" "URE based hypergraph pattern miner" HAVE_MINER)

SUMMARY_SHOW()
