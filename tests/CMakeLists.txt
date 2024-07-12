# This is the CMake file for the test directory in the UPP
# project. 
#
# Ed Hartnett, 7/11/24

# This function builds, links, and runs a test program.
function(create_test name)
  add_executable(${name} ${name}.F90)
  add_test(NAME ${name} COMMAND ${name})
  target_link_libraries(${name} PRIVATE upp)  
endfunction()

create_test(t1)

