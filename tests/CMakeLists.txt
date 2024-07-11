# This is the CMake file for the test directory in the UPP
# project. 
#
# Ed Hartnett, 7/11/24

# This function builds, links, and runs a test program.
function(create_test name kind)
  add_executable(${name}_${kind} ${name}.F90)
  add_test(NAME ${name}_${kind} COMMAND ${name}_${kind})
endfunction()

# Build a _4 and _d version of each test and link them to the _4 and
# _d builds of the library, for 4-byte real, and 8-byte real.
foreach(kind ${kinds})
  if (${kind} EQUAL "4")
    create_test(t1 ${kind})
  endif()
endforeach()

