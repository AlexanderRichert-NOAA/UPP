# This is the CMake file for the test directory in the UPP
# project. 
#
# Ed Hartnett, 7/11/24

# Include cmake functions for MPI.
include(LibMPI)
set(DEFAULT_TEST_TIMEOUT 600)

# This function builds, links, and runs a test program.
function(create_test name)
  add_executable(${name} ${name}.F90)
  add_test(NAME ${name} COMMAND ${name})
  target_link_libraries(${name} PRIVATE upp)  
endfunction()

# This function builds, links, and runs a test program.
function(create_mpi_test name)
  add_executable(${name} ${name}.F90)
  target_link_libraries(${name} PRIVATE upp)  
  add_mpi_test(${name}
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/${name}
    NUMPROCS 4
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
endfunction()

create_test(t1)
create_mpi_test(t2)
create_mpi_test(t3)



