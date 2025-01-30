SET(GENERIC_LIB_VERSION "1.0")
SET(GENERIC_LIB_SOVERSION "1")


SET(COMMON_SRC
    common/error_message.c
    common/backtrace.c
    common/counter.c
    common/string.c
    common/posix_impl.c
    common/fortran_api.f90
    common/statistics.f90

    table/table.c
    table/table_comment.c
    table/table_latex.c
    table/table.f90
    table/ops.c

    ds/ds.c
    ds/hashtable.c
    ds/slist.c
    ds/map.c

    sysinfo/getpid.c
    sysinfo/meminfo.c
    sysinfo/cpucount.c
    sysinfo/cpufreq.c
    sysinfo/hostname.c
    sysinfo/uname.c
    sysinfo/cpu_name.c
    sysinfo/cpuinfo.c
    sysinfo/compiler_info.c
    sysinfo/fortran_api.f90
    sysinfo/cmake.c
    sysinfo/compilerflags.c
    sysinfo/cpu_features.c
    )

IF( LUA_ENABLE)
    SET(LUA_SRC
        lua/interface.c
        lua/fortran_interface.f90
        )
ENDIF()

IF(IO_ENABLE)
    SET(IO_SRC
        file/compress_handler.c
        file/compress_io_bzip.c
        file/compress_io_gzip.c
        file/compress_io_uncompressed.c
        file/compress_io_xz.c
        file/io.c
        file/dir.c
        file/mtx_write.c
        file/mtx_read.c
        file/mtx_api.f90
        )
ENDIF()

IF(INIFILE_ENABLE)
    SET(INI_SRC
        ini/inifile.c
        ini/easy_access.c
        ini/fortran_api.f90
        )
ENDIF()

IF(THREADING_ENABLE)
    SET(THREAD_SRC
        threading/worker_queue.c
        threading/bgthread.c)
ENDIF()

IF(HDF5_ENABLE)
    SET(HDF5_SRC
        hdf5/common.c
        hdf5/vector.c
        hdf5/matrix.c
        hdf5/datatypes.c
        hdf5/sparse_matrix.c
        hdf5/fortran_api.F90
        hdf5/filters/filter_bzip2.c
        hdf5/filters/filter_xz.c
        hdf5/filters/filter_zstd.c
        hdf5/attribute.c
        hdf5/group.c
        hdf5/dataset.c
        hdf5/open_close.c
        )
ENDIF()

IF(IMAGE_ENABLE)
    SET(IMAGE_SRC
        image/bmp.c)
ENDIF()

IF(TRACE_ENABLE)
    SET(TRACE_SRC
        eventtracer/reset.c
        eventtracer/write.c
        eventtracer/record_event.c
        eventtracer/fortran_api.f90
        )
ENDIF()

SET(SRC ${COMMON_SRC} ${FILE_SRC} ${IO_SRC} ${INI_SRC} ${THREAD_SRC} ${HDF5_SRC} ${IMAGE_SRC} ${DS_SRC} ${TRACE_SRC})

INCLUDE_DIRECTORIES(${INCLUDE_DIR})
#INCLUDE_DIRECTORIES(${PROJECT_BINARY_DIR}/include)
INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR}/src)
INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR}/include)

IF ( LUA_ENABLE )
    ADD_LIBRARY(cscutils_lua_src OBJECT ${LUA_SRC} common/error_message.c)
    IF (NOT STANDALONE)
        IF (CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
            SET_TARGET_PROPERTIES(cscutils_lua_src PROPERTIES C_STANDARD 11 C_VISIBILITY_PRESET hidden)
        ELSE()
            SET_TARGET_PROPERTIES(cscutils_lua_src PROPERTIES C_STANDARD 11 C_VISIBILITY_PRESET hidden Fortran_VISIBILITY_PRESET hidden)
        ENDIF()
    ENDIF()

    TARGET_INCLUDE_DIRECTORIES(cscutils_lua_src PUBLIC
        ${INCLUDE_DIR}
        $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src>
        $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>
        $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/src>
        $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/include>
        $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/include/cscutils>
        )


ENDIF()

ADD_LIBRARY(cscutils_src OBJECT ${SRC})

if(${CMAKE_SYSTEM_NAME} STREQUAL Linux)
    # set_source_files_properties(sysinfo/cmake.c PROPERTIES COMPILE_DEFINITIONS CMAKE_ARGS="${CMAKE_ARGS}")
endif()

get_property(elg GLOBAL PROPERTY ENABLED_LANGUAGES)
FOREACH ( L ${elg})
    STRING(TOUPPER ${CMAKE_BUILD_TYPE} BT)
    set_property(SOURCE sysinfo/compilerflags.c APPEND PROPERTY COMPILE_DEFINITIONS CMAKE_${L}_FLAGS="${CMAKE_${L}_FLAGS} ${CMAKE_${L}_FLAGS_${BT}}")
    set_property(SOURCE sysinfo/compilerflags.c APPEND PROPERTY COMPILE_DEFINITIONS LANGUAGE_${L}=1)
ENDFOREACH()


IF(NOT STANDALONE)
    IF (CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
        SET_TARGET_PROPERTIES(cscutils_src PROPERTIES C_STANDARD 11 C_VISIBILITY_PRESET hidden )
    ELSE()
        SET_TARGET_PROPERTIES(cscutils_src PROPERTIES C_STANDARD 11 C_VISIBILITY_PRESET hidden Fortran_VISIBILITY_PRESET hidden)
    ENDIF()
ENDIF()



# IF( INPROJECT AND CSCUTILS_OBJECT )
    # TARGET_LINK_LIBRARIES(cscutils_src PUBLIC ${LIBS})
#
# ELSE()
    IF(LUA_ENABLE)
        ADD_LIBRARY(cscutils $<TARGET_OBJECTS:cscutils_src> $<TARGET_OBJECTS:cscutils_lua_src>)
        TARGET_LINK_LIBRARIES(cscutils lua  ${LIBS})
    ELSE()
        ADD_LIBRARY(cscutils $<TARGET_OBJECTS:cscutils_src>)
        TARGET_LINK_LIBRARIES(cscutils  ${LIBS})
    ENDIF()
    TARGET_INCLUDE_DIRECTORIES(cscutils PUBLIC
        ${INCLUDE_DIR}
        $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src>
        $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>
        $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/src>
        $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/include>
        $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/include/cscutils>
        )
    TARGET_INCLUDE_DIRECTORIES(cscutils_src PUBLIC
        ${INCLUDE_DIR}
        $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src>
        $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>
        $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/src>
        $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/include>
        $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/include/cscutils>
        )

    SET_TARGET_PROPERTIES(cscutils PROPERTIES OUTPUT_NAME "cscutils" C_STANDARD 11
        )

    IF(WIN32)
        TARGET_LINK_LIBRARIES(cscutils wsock32)
        TARGET_LINK_LIBRARIES(cscutils ws2_32)
    ENDIF()
# ENDIF()

# Build Shared Library if we do not use a stand alone build
IF(NOT INPROJECT)
    ADD_LIBRARY(cscutils_shared $<TARGET_OBJECTS:cscutils_src>)
    TARGET_LINK_LIBRARIES(cscutils_shared  ${LIBS})
    SET_TARGET_PROPERTIES(cscutils_shared  PROPERTIES VERSION ${GENERIC_LIB_VERSION} SOVERSION ${GENERIC_LIB_SOVERSION} OUTPUT_NAME "cscutils" C_STANDARD 11)
ENDIF()




IF(NOT INPROJECT)
    INSTALL(TARGETS cscutils cscutils_shared
        RUNTIME DESTINATION bin COMPONENT bin
        LIBRARY DESTINATION lib COMPONENT shlib
        ARCHIVE DESTINATION lib COMPONENT stlib
        PUBLIC_HEADER DESTINATION include COMPONENT dev)

    INSTALL(DIRECTORY ${PROJECT_SOURCE_DIR}/include/cscutils/ DESTINATION include/cscutils
        FILES_MATCHING PATTERN "*.h")
ENDIF()
