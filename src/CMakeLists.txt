
file(GLOB_RECURSE fgputensor_FSRC "${CMAKE_CURRENT_SOURCE_DIR}/*.f*")

# Enable pre-processing for source code
set_source_files_properties(
  ${fgputensor_FSRC}
  PROPERTIES Fortran_PREPROCESS ON
)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)


# Shared library installation
add_library(fgputensor SHARED ${fgputensor_FSRC})
if( hip_FOUND )
        target_link_libraries(fgputensor PRIVATE hip::device)
endif()
target_compile_options(fgputensor PRIVATE -fPIC)
set_target_properties(fgputensor PROPERTIES LINKER_LANGUAGE Fortran)
#set_target_properties(fgputensor PROPERTIES PUBLIC_HEADER ${fgputensor_HEADERS})
install(TARGETS fgputensor
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
        PUBLIC_HEADER DESTINATION include)


# Static library installation
add_library(fgputensor-static STATIC ${fgputensor_FSRC})
if( hip_FOUND )
        target_link_libraries(fgputensor-static PRIVATE hip::device)
endif()
set_target_properties(fgputensor-static PROPERTIES LINKER_LANGUAGE Fortran)
#set_target_properties(fgputensor-static PROPERTIES PUBLIC_HEADER ${fgputensor_HEADERS})

install(TARGETS fgputensor-static
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
        PUBLIC_HEADER DESTINATION include)
        
# Install the .mod files under include/
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION include)