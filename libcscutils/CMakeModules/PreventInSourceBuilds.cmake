#
# This function will prevent in-source builds
function(assureoutofsourcebuilds)
    # make sure the user doesn't play dirty with symlinks
    get_filename_component(srcdir "${CMAKE_SOURCE_DIR}" REALPATH)
    get_filename_component(bindir "${CMAKE_BINARY_DIR}" REALPATH)

    # disallow in-source builds
    if("${srcdir}" STREQUAL "${bindir}")
        message(FATAL_ERROR "Quitting configuration -- In-source build is not possible.")
    endif()
endfunction()

assureoutofsourcebuilds()
