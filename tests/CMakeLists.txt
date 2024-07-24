# This is the CMake file for the test directory in the UPP
# project. 
#
# Ed Hartnett, 7/11/24

# Include cmake functions for MPI.
include(LibMPI)

# Set timeout for MPI runs in seconds.
set(DEFAULT_TEST_TIMEOUT 60)

# Some very small test files may be committed to the repo. This
# function copies such a data file to the build directory.
function(copy_test_data name)
  message(STATUS "Copying test file ${name}")
  file(COPY "${CMAKE_CURRENT_SOURCE_DIR}/data/${name}"
    DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/data"
    FILE_PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)
endfunction()

# This function builds, links, and runs a test program.
function(create_test name)
  add_executable(${name} ${name}.F90)
  add_test(NAME ${name} COMMAND ${name})
  target_link_libraries(${name} PRIVATE upp)  
endfunction()

# This function builds, links, and runs an MPI test program. The
# arguments are name of test and number of procs to run it on.
function(create_mpi_test name num_procs)
  add_executable(${name}_${num_procs} ${name}.F90)
  target_link_libraries(${name}_${num_procs} PRIVATE upp PRIVATE g2::g2_4)  
  add_mpi_test(${name}_${num_procs}_procs
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/${name}_${num_procs}
    NUMPROCS ${num_procs}
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
endfunction()

# Copye params_grib2_tbl_new to test dir.
message(STATUS "Copying test file ${name}")
file(COPY "${CMAKE_CURRENT_SOURCE_DIR}/data/params_grib2_tbl_new"
  DESTINATION "${CMAKE_CURRENT_BINARY_DIR}"
  FILE_PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)

  # Copy test data files to build directory.
copy_test_data(tocgrib2.nml)

# This test is run without MPI.
create_test(t1)

# These tests are run with MPI.
create_mpi_test(t2 4)
create_mpi_test(t3 1)
#create_mpi_test(t3 4)



