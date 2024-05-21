#
# Wrappper to compile LUA
#

CMAKE_MINIMUM_REQUIRED(VERSION 3.5)

PROJECT(Lua LANGUAGES C)

INCLUDE(SetPolicy)
SET_POLICY(CMP0075 NEW)
SET_POLICY(CMP0063 NEW)
SET_POLICY(CMP0069 NEW)

SET(SRC_CORE
    src/lapi.c
    src/lcode.c
    src/lctype.c
    src/ldebug.c
    src/ldo.c
    src/ldump.c
    src/lfunc.c
    src/lgc.c
    src/llex.c
    src/lmem.c
    src/lobject.c
    src/lopcodes.c
    src/lparser.c
    src/lstate.c
    src/lstring.c
    src/ltable.c
    src/ltm.c
    src/lundump.c
    src/lvm.c
    src/lzio.c
)

SET(SRC_LIB
    src/lauxlib.c
    src/lbaselib.c
    src/lcorolib.c
    src/ldblib.c
    src/liolib.c
    src/lmathlib.c
    src/loadlib.c
    src/loslib.c
    src/lstrlib.c
    src/ltablib.c
    src/lutf8lib.c
    src/linit.c
    )

ADD_LIBRARY(lua_src OBJECT ${SRC_CORE} ${SRC_LIB})
ADD_LIBRARY(lua STATIC $<TARGET_OBJECTS:lua_src>)



IF (CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
    SET_TARGET_PROPERTIES(lua_src PROPERTIES C_STANDARD 11 C_VISIBILITY_PRESET hidden INTERPROCEDURAL_OPTIMIZATION FALSE)
ELSE()
    SET_TARGET_PROPERTIES(lua_src PROPERTIES C_STANDARD 11 C_VISIBILITY_PRESET hidden Fortran_VISIBILITY_PRESET hidden INTERPROCEDURAL_OPTIMIZATION FALSE)
ENDIF()

IF (CMAKE_BUILD_TYPE STREQUAL "Debug")
    SET(LUA_C_FLAGS "LUA_COMPAT_5_3;LUA_USE_APICHECK;LUA_USE_JUMPTABLE=0")
ELSE ()
    SET(LUA_C_FLAGS "LUA_COMPAT_5_3;LUA_USE_JUMPTABLE=0")
ENDIF()
IF (CMAKE_SYSTEM_NAME STREQUAL "FreeBSD" OR
    CMAKE_SYSTEM_NAME STREQUAL "NetBSD" OR
    CMAKE_SYSTEM_NAME STREQUAL "OpenBSD" OR
    CMAKE_SYSTEM_NAME STREQUAL "DragonflyBSD")
    LIST(LUA_C_FLAGS APPEND "LUA_USE_LINUX")
ENDIF()

IF (CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    LIST(LUA_C_FLAGS APPEND "LUA_USE_MACOSX")
ENDIF()


SET_TARGET_PROPERTIES(lua_src PROPERTIES POSITION_INDEPENDENT_CODE True COMPILE_DEFINITIONS "${LUA_C_FLAGS}")
SET_TARGET_PROPERTIES(lua_src PROPERTIES ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/lib")
FILE(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/include")
FILE(COPY src/lua.h src/luaconf.h src/lualib.h src/lauxlib.h src/lua.hpp DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/include")




