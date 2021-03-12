list( APPEND c_sw_src_files
netCDFModule.f90
print_affinity.c
runnable.c
sw_core.F90
sw_driver.F90
)

add_executable( ${PROJECT_NAME} ${c_sw_src_files} )

target_include_directories( ${PROJECT_NAME} SYSTEM PUBLIC ${NetCDF_INCLUDE_DIRS} )

target_link_libraries(${PROJECT_NAME} PUBLIC NetCDF::NetCDF_Fortran)
if(OpenMP_FOUND)
    target_link_libraries(${PROJECT_NAME} PUBLIC OpenMP::OpenMP_Fortran)
    target_link_libraries(${PROJECT_NAME} PUBLIC OpenMP::OpenMP_C)
endif()

if ( GPTL_FOUND )
   target_link_libraries( ${PROJECT_NAME} PUBLIC GPTL::GPTL )
endif()

if ( MPI_FOUND )
  target_link_libraries( ${PROJECT_NAME} PUBLIC MPI::MPI_Fortran )
  target_link_libraries( ${PROJECT_NAME} PUBLIC MPI::MPI_C )
endif()

install(TARGETS c_sw
        DESTINATION ${PROJECT_SOURCE_DIR}/exe)
