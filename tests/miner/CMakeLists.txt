# Special unit test atom types, tested by the FactoryUTest
OPENCOG_ADD_ATOM_TYPES(
   test_types.script
   test_types.h
   test_types.definitions
   test_types.inheritance
   test_types.scm
   test_types.pyx
)
ADD_CUSTOM_TARGET(test_atom_types DEPENDS test_types.h)

# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

ADD_LIBRARY (test-types SHARED
   test_types.h
   TestTypes.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(test-types test_atom_types)

TARGET_LINK_LIBRARIES(test-types
   ${ATOMSPACE_atomtypes_LIBRARY}
)

ADD_LIBRARY(minerutestutils SHARED
  MinerUTestUtils.cc
)

TARGET_LINK_LIBRARIES(minerutestutils
  miner
  guile-miner
  ${URE_LIBRARIES}
  ${UNIFY_LIBRARIES}
)

ADD_CXXTEST(ValuationsUTest)
TARGET_LINK_LIBRARIES(ValuationsUTest
  test-types
  miner
  guile-miner
  ${URE_LIBRARIES}
  ${UNIFY_LIBRARIES}
)

ADD_CXXTEST(MinerUTest)
TARGET_LINK_LIBRARIES(MinerUTest
  test-types
  minerutestutils
)

ADD_CXXTEST(SurprisingnessUTest)
TARGET_LINK_LIBRARIES(SurprisingnessUTest
  minerutestutils
)
