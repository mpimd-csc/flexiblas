#
# Wrappper to compile LUA
#

CMAKE_MINIMUM_REQUIRED(VERSION 3.0)

PROJECT(Lua LANGUAGES C)

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
    src/lbitlib.c
    src/lcorolib.c
    src/ldblib.c
    src/liolib.c
    src/lmathlib.c
    src/loslib.c
    src/lstrlib.c
    src/ltablib.c
    src/lutf8lib.c
    src/loadlib.c
    src/linit.c
)

ADD_LIBRARY(lua STATIC ${SRC_CORE} ${SRC_LIB})
SET_TARGET_PROPERTIES(lua PROPERTIES POSITION_INDEPENDENT_CODE True COMPILE_DEFINITIONS "LUA_COMPAT_5_2")
SET_TARGET_PROPERTIES(lua PROPERTIES ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/lib")
FILE(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/include")
FILE(COPY src/lua.h src/luaconf.h src/lualib.h src/lauxlib.h src/lua.hpp DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/include")




