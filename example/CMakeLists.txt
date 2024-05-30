CMAKE_MINIMUM_REQUIRED(VERSION 3.0.2)

set(CMAKE_VERBOSE_MAKEFILE ON)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)
set(make_env FGPUTENSOR_HOME=${CMAKE_BINARY_DIR} FGPUTENSOR_COMPILER=${FGPUTENSOR_COMPILER})


function (add_fortran_examples)

    foreach (TFILE ${ARGN})
        string (REGEX REPLACE ".f90$" "" EXAMPLE_NAME ${TFILE})
        add_executable (${EXAMPLE_NAME} ${CMAKE_CURRENT_SOURCE_DIR}/${TFILE})
        target_link_libraries(${EXAMPLE_NAME}
                    ${CMAKE_BINARY_DIR}/src/libfgputensor-static.a
                    hip::device)
                           
        target_include_directories(${EXAMPLE_NAME} PRIVATE 
                        ${CMAKE_BINARY_DIR}/include)

        add_test(NAME ${EXAMPLE_NAME} COMMAND ${EXAMPLE_NAME})
        install(TARGETS ${EXAMPLE_NAME} DESTINATION example)
    endforeach ()

endfunction ()

add_fortran_examples (
  "simple_2d_array.f90"
)
