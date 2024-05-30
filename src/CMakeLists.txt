
file(GLOB_RECURSE fgputensor_FSRC "${CMAKE_CURRENT_SOURCE_DIR}/*.f*")

# Enable pre-processing for source code
set_source_files_properties(
  ${fgputensor_FSRC}
  PROPERTIES Fortran_PREPROCESS ON
)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)


# Shared library installation
add_library(fgputensor SHARED ${fgputensor_FSRC})
target_link_libraries(fgputensor PRIVATE hip::device)
target_compile_options(fgputensor PRIVATE -fPIC)
set_target_properties(fgputensor PROPERTIES LINKER_LANGUAGE Fortran)
#set_target_properties(fgputensor PROPERTIES PUBLIC_HEADER ${fgputensor_HEADERS})
install(TARGETS fgputensor
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
        PUBLIC_HEADER DESTINATION include)


# Static library installation
add_library(fgputensor-static STATIC ${fgputensor_FSRC})
target_link_libraries(fgputensor-static PRIVATE hip::device)
set_target_properties(fgputensor-static PROPERTIES LINKER_LANGUAGE Fortran)
#set_target_properties(fgputensor-static PROPERTIES PUBLIC_HEADER ${fgputensor_HEADERS})

install(TARGETS fgputensor-static
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
        PUBLIC_HEADER DESTINATION include)

# target_include_directories(fgputensor-static PRIVATE 
#                         ${FEQPARSE_INCLUDE_DIRS}
#                         ${HDF5_INCLUDE_DIRS}
#                         ${MPI_Fortran_INCLUDE_DIRS}
#                         ${HIPFORT_INCLUDE_DIRS})





# Install the .mod files under include/
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION include)