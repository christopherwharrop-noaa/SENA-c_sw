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

# 12 x 24 workload
add_test(NAME regression_12x24
         COMMAND bash -c "set -o pipefail; ../src/c_sw test_input/c_sw_12x24.nl 2>&1 | tee test_output/c_sw_12x24.log.out")
add_test(NAME compare_12x24
         COMMAND ${COMPARE_SH} test_output/c_sw_12x24.log.out test_output/c_sw_12x24.test)

# 24 x 24 workload
add_test(NAME regression_24x24
         COMMAND bash -c "set -o pipefail; ../src/c_sw test_input/c_sw_24x24.nl 2>&1 | tee test_output/c_sw_24x24.log.out")
add_test(NAME compare_24x24
         COMMAND ${COMPARE_SH} test_output/c_sw_24x24.log.out test_output/c_sw_24x24.test)

# 48 x 24 workload
add_test(NAME regression_48x24
         COMMAND bash -c "set -o pipefail; ../src/c_sw test_input/c_sw_48x24.nl 2>&1 | tee test_output/c_sw_48x24.log.out")
add_test(NAME compare_48x24
         COMMAND ${COMPARE_SH} test_output/c_sw_48x24.log.out test_output/c_sw_48x24.test)

# 48 x 48 workload
add_test(NAME regression_48x48
         COMMAND bash -c "set -o pipefail; ../src/c_sw test_input/c_sw_48x48.nl 2>&1 | tee test_output/c_sw_48x48.log.out")
add_test(NAME compare_48x48
         COMMAND ${COMPARE_SH} test_output/c_sw_48x48.log.out test_output/c_sw_48x48.test)

