SET(SRC
    demo_inifile
    demo_io
    demo_table
    demo_sysinfo
    )
SET(FSRC
    demo_table_fortran
)

IF(LUA_ENABLE)
    SET(SRC ${SRC} demo_lua)
    SET(FSRC ${FSRC} demo_lua_f)


ENDIF()

IF(HDF5_ENABLE)
    INCLUDE_DIRECTORIES(${HDF5_INCLUDE_DIRS})
    INCLUDE_DIRECTORIES(${PROJECT_BINARY_DIR}/src)

    SET(SRC ${SRC} demo_hdf demo_hdf5_read)
    SET(FSRC ${FSRC} demo_hdf_f)


ENDIF()

IF(DS_ENABLE)
    SET(SRC ${SRC} demo_hashtable
        demo_slist
        demo_map

        )

ENDIF()
IF(THREADING_ENABLE)
    SET(SRC ${SRC}
        demo_background_worker)
ENDIF()

INCLUDE(${PROJECT_SOURCE_DIR}/src/lua/LuaInterface.cmake)

INCLUDE_DIRECTORIES(${PROJECT_BINARY_DIR}/include)
INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR}/include)
FOREACH(srcfile ${SRC})
    ADD_EXECUTABLE(${srcfile} ${srcfile}.c)
    TARGET_LINK_LIBRARIES(${srcfile} cscutils ${LIBS})
ENDFOREACH()

IF (CMAKE_Fortran_COMPILER_LOADED)
    FOREACH(srcfile ${FSRC})
        ADD_EXECUTABLE(${srcfile} ${srcfile}.f90)
        TARGET_LINK_LIBRARIES(${srcfile} cscutils ${LIBS})
    ENDFOREACH()
ENDIF()

CSC_LUA_ADD_INLINE_CODE(demo_lua ${CMAKE_CURRENT_SOURCE_DIR}/demo_lua_incode.lua VARNAME incode FUNCTIONNAME incode_func)

