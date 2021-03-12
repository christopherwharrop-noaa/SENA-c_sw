add_subdirectory( tools )

list( APPEND c_sw_test_data
  data/inputs/c_sw_12x24.nc
  data/inputs/c_sw_24x24.nc
  data/inputs/c_sw_48x24.nc
  data/inputs/c_sw_48x48.nc
)

list( APPEND c_sw_test_input
  test_input/c_sw_12x24.nl
  test_input/c_sw_24x24.nl
  test_input/c_sw_48x24.nl
  test_input/c_sw_48x48.nl
)

list( APPEND c_sw_test_output
  test_output/c_sw_12x24.test
  test_output/c_sw_24x24.test
  test_output/c_sw_48x24.test
  test_output/c_sw_48x48.test
)

# Create data directory for kernel input and symlink all files
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/data)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/data/inputs)
foreach(FILENAME ${c_sw_test_data})
    execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
           ${CMAKE_CURRENT_SOURCE_DIR}/../../${FILENAME}
           ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME} )
endforeach(FILENAME)

# Create empty data directory for model test output
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/data/outputs)

# Create test_input directory for test input and symlink all files
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/test_input)
foreach(FILENAME ${c_sw_test_input})
    execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
           ${CMAKE_CURRENT_SOURCE_DIR}/${FILENAME}
           ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME} )
endforeach(FILENAME)

# Create data directory for reference output and symlink all files
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/test_output)
foreach(FILENAME ${c_sw_test_output})
    execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
           ${CMAKE_CURRENT_SOURCE_DIR}/${FILENAME}
           ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME} )
endforeach(FILENAME)

set(COMPARE_SH ${CMAKE_BINARY_DIR}/bin/c_sw_compare.sh)

# Tests when MPI is enabled
if ( ENABLE_MPI )

  # 12 x 24 workload
  add_test(NAME regression_12x24
           COMMAND bash -c "${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} 1 ${MPIEXEC_PREFLAGS} ../src/c_sw test_input/c_sw_12x24.nl")
  add_test(NAME compare_12x24
           COMMAND ${COMPARE_SH} test_output/c_sw_12x24.log.0000 test_output/c_sw_12x24.test)

  # 24 x 24 workload
  add_test(NAME regression_24x24
           COMMAND bash -c "${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} 1 ${MPIEXEC_PREFLAGS} ../src/c_sw test_input/c_sw_24x24.nl")
  add_test(NAME compare_24x24
           COMMAND ${COMPARE_SH} test_output/c_sw_24x24.log.0000 test_output/c_sw_24x24.test)

  # 48 x 24 workload
  add_test(NAME regression_48x24
           COMMAND bash -c "${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} 1 ${MPIEXEC_PREFLAGS} ../src/c_sw test_input/c_sw_48x24.nl")
  add_test(NAME compare_48x24
           COMMAND ${COMPARE_SH} test_output/c_sw_48x24.log.0000 test_output/c_sw_48x24.test)

  # 48 x 48 workload
  add_test(NAME regression_48x48
           COMMAND bash -c "${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} 1 ${MPIEXEC_PREFLAGS} ../src/c_sw test_input/c_sw_48x48.nl")
  add_test(NAME compare_48x48
           COMMAND ${COMPARE_SH} test_output/c_sw_48x48.log.0000 test_output/c_sw_48x48.test)

  # 12 x 24 workload with 4 MPI ranks
  add_test(NAME regression_12x24_procs4
           COMMAND bash -c "${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} 4 ${MPIEXEC_PREFLAGS} ../src/c_sw test_input/c_sw_12x24.nl")
  add_test(NAME compare_12x24_rank0
           COMMAND ${COMPARE_SH} test_output/c_sw_12x24.log.0000 test_output/c_sw_12x24.test)
  add_test(NAME compare_12x24_rank1
           COMMAND ${COMPARE_SH} test_output/c_sw_12x24.log.0001 test_output/c_sw_12x24.test)
  add_test(NAME compare_12x24_rank2
           COMMAND ${COMPARE_SH} test_output/c_sw_12x24.log.0002 test_output/c_sw_12x24.test)
  add_test(NAME compare_12x24_rank3
           COMMAND ${COMPARE_SH} test_output/c_sw_12x24.log.0003 test_output/c_sw_12x24.test)

  # 24 x 24 workload with 4 MPI ranks
  add_test(NAME regression_24x24_procs4
           COMMAND bash -c "${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} 4 ${MPIEXEC_PREFLAGS} ../src/c_sw test_input/c_sw_24x24.nl")
  add_test(NAME compare_24x24_rank0
           COMMAND ${COMPARE_SH} test_output/c_sw_24x24.log.0000 test_output/c_sw_24x24.test)
  add_test(NAME compare_24x24_rank1
           COMMAND ${COMPARE_SH} test_output/c_sw_24x24.log.0001 test_output/c_sw_24x24.test)
  add_test(NAME compare_24x24_rank2
           COMMAND ${COMPARE_SH} test_output/c_sw_24x24.log.0002 test_output/c_sw_24x24.test)
  add_test(NAME compare_24x24_rank3
           COMMAND ${COMPARE_SH} test_output/c_sw_24x24.log.0003 test_output/c_sw_24x24.test)

  # 48 x 24 workload with 4 MPI ranks
  add_test(NAME regression_48x24_procs4
           COMMAND bash -c "${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} 4 ${MPIEXEC_PREFLAGS} ../src/c_sw test_input/c_sw_48x24.nl")
  add_test(NAME compare_48x24_rank0
           COMMAND ${COMPARE_SH} test_output/c_sw_48x24.log.0000 test_output/c_sw_48x24.test)
  add_test(NAME compare_48x24_rank1
           COMMAND ${COMPARE_SH} test_output/c_sw_48x24.log.0001 test_output/c_sw_48x24.test)
  add_test(NAME compare_48x24_rank2
           COMMAND ${COMPARE_SH} test_output/c_sw_48x24.log.0002 test_output/c_sw_48x24.test)
  add_test(NAME compare_48x24_rank3
           COMMAND ${COMPARE_SH} test_output/c_sw_48x24.log.0003 test_output/c_sw_48x24.test)

  # 48 x 48 workload with 4 MPI ranks
  add_test(NAME regression_48x48_procs4
           COMMAND bash -c "${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} 4 ${MPIEXEC_PREFLAGS} ../src/c_sw test_input/c_sw_48x48.nl")
  add_test(NAME compare_48x48_rank0
           COMMAND ${COMPARE_SH} test_output/c_sw_48x48.log.0000 test_output/c_sw_48x48.test)
  add_test(NAME compare_48x48_rank1
           COMMAND ${COMPARE_SH} test_output/c_sw_48x48.log.0001 test_output/c_sw_48x48.test)
  add_test(NAME compare_48x48_rank2
           COMMAND ${COMPARE_SH} test_output/c_sw_48x48.log.0002 test_output/c_sw_48x48.test)
  add_test(NAME compare_48x48_rank3
           COMMAND ${COMPARE_SH} test_output/c_sw_48x48.log.0003 test_output/c_sw_48x48.test)

# Tests when MPI is not enabled
else()

  # 12 x 24 workload
  add_test(NAME regression_12x24
           COMMAND bash -c "../src/c_sw test_input/c_sw_12x24.nl")
  add_test(NAME compare_12x24
           COMMAND ${COMPARE_SH} test_output/c_sw_12x24.log test_output/c_sw_12x24.test)

  # 24 x 24 workload
  add_test(NAME regression_24x24
           COMMAND bash -c "../src/c_sw test_input/c_sw_24x24.nl")
  add_test(NAME compare_24x24
           COMMAND ${COMPARE_SH} test_output/c_sw_24x24.log test_output/c_sw_24x24.test)

  # 48 x 24 workload
  add_test(NAME regression_48x24
           COMMAND bash -c "../src/c_sw test_input/c_sw_48x24.nl")
  add_test(NAME compare_48x24
           COMMAND ${COMPARE_SH} test_output/c_sw_48x24.log test_output/c_sw_48x24.test)

  # 48 x 48 workload
  add_test(NAME regression_48x48
           COMMAND bash -c "../src/c_sw test_input/c_sw_48x48.nl")
  add_test(NAME compare_48x48
           COMMAND ${COMPARE_SH} test_output/c_sw_48x48.log test_output/c_sw_48x48.test)

endif()
