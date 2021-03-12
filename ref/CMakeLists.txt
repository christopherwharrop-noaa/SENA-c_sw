################################################################################
# c_sw
################################################################################

cmake_minimum_required( VERSION 3.10 )

project( c_sw LANGUAGES Fortran C )

list( APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake )
set( CMAKE_DIRECTORY_LABELS ${PROJECT_NAME} )

option( ENABLE_GPTL "Use GPTL profiling library (if available)" OFF )
option( ENABLE_AUTOPROFILING "Enable function-based autoprofiling with GPTL (if available)" OFF )
option( ENABLE_MPI "Simulate MPI execution" OFF )

enable_testing()

include(${PROJECT_NAME}_compiler_flags)

find_package( OpenMP COMPONENTS C Fortran )
find_package( NetCDF REQUIRED COMPONENTS C Fortran )

if ( ENABLE_MPI )
  find_package( MPI COMPONENTS C Fortran)
  add_compile_definitions(ENABLE_MPI)
endif()

if ( ENABLE_GPTL )
  find_package( GPTL )
  add_compile_definitions(ENABLE_GPTL)
endif()

if ( GPTL_FOUND AND ENABLE_AUTOPROFILING )
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${Fortran_AUTOPROFILING_FLAGS}")
endif()

add_subdirectory(src)
add_subdirectory(test)

