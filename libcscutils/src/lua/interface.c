/*
 * LIBCSCUTILS - LUA Interface
 * Copyright (C) Martin Koehler, 2020
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, see <http://www.gnu.org/licenses/>.
 *
 */



#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "luaconf.h"
#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"

#include "cscutils/error_message.h"
#include "cscutils/lua.h"


/* Stack dump helper. Only for internal usage.  */
__attribute__((unused))
static void stackDump (lua_State *L) {
    int i;
    int top = lua_gettop(L);
    printf("Number of elements: %d\n", top);
    for (i = 1; i <= top; i++) {  /* repeat for each level */
        printf("%5d %5d\t", i, -top+i-1);
        int t = lua_type(L, i);
        switch (t) {

            case LUA_TSTRING:  /* strings */
                printf("STRING -- `%s'", lua_tostring(L, i));
                break;

            case LUA_TBOOLEAN:  /* booleans */
                printf("BOOL   -- %s", lua_toboolean(L, i) ? "true" : "false");
                break;

            case LUA_TNUMBER:  /* numbers */
                printf("NUMBER -- %g", lua_tonumber(L, i));
                break;

            default:  /* other values */
                printf("OTHER  -- %s", lua_typename(L, t));
                break;

        }
        printf("\n");  /* put a separator */
    }
    printf("\n");  /* end the listing */
}



csc_lua_t csc_lua_init()
{
    lua_State *state;
    state = luaL_newstate();

    if (!state) {
        csc_error_message("LUA initialization failed.\n");
        return NULL;
    }
    luaL_openlibs(state);
    return (csc_lua_t) state;
}

void csc_lua_finalize(csc_lua_t lua)
{
    lua_State *state = lua;
    if ( state) lua_close(state);
    state = NULL;

}

int csc_lua_loadfile(csc_lua_t lua, const char * filename)
{
    int err;
    lua_State *state = lua;

    if (!filename) {
        return CSC_ERROR;
    }
    if (!state) {
        return CSC_ERROR;
    }

    err =  luaL_loadfile(state, filename);
    /* stackDump(state); */

    if ( err == LUA_ERRSYNTAX) {
        csc_error_message("Load LUA file %s failed -- Error: %s\n", filename, lua_tostring(state, -1));
        return CSC_ERROR;
    }
    if ( err != LUA_OK) {
        csc_error_message("Failed to load lua file: %s\n", filename);
        return CSC_ERROR;
    }
    return CSC_SUCCESS;
}

int csc_lua_loadstring(csc_lua_t lua, const char * code )
{
    int err;
    lua_State *state = lua;

    if (!code) {
        return CSC_ERROR;
    }
    if (!state) {
        return CSC_ERROR;
    }

    err = luaL_loadstring(state, code);
    /* stackDump(state); */

    if ( err == LUA_ERRSYNTAX ) {
        csc_error_message("Load LUA string failed -- Error: %s\n", lua_tostring(state, -1));
        return CSC_ERROR;
    }
    if ( err != LUA_OK) {
        csc_error_message("Failed to load lua string.\n");
        return CSC_ERROR;
    }
    return CSC_SUCCESS;
}

int csc_lua_run(csc_lua_t lua)
{
    lua_State *state = lua;

    if ( lua_pcall(state, 0, 0,0) ) {
        csc_error_message("Failed to run loaded lua scripts.\n");
        return CSC_ERROR;
    }
    return CSC_SUCCESS;
}

/* Set Globals  */
int csc_lua_global_int(csc_lua_t lua, const char *name, int value)
{
    lua_State *state = lua;

    if ( ! state ) {
        return CSC_ERROR;
    }

    lua_pushinteger(state, value);
    lua_setglobal(state, name);

    return CSC_SUCCESS;
}

int csc_lua_global_double(csc_lua_t lua, const char *name, double value)
{
    lua_State *state = lua;
    if ( ! state ) {
        return CSC_ERROR;
    }

    lua_pushnumber(state, value);
    lua_setglobal(state, name);

    return CSC_SUCCESS;
}

int csc_lua_global_string(csc_lua_t lua, const char *name, const char *value)
{
    lua_State *state = lua;
    if ( ! state ) {
        return CSC_ERROR;
    }

    lua_pushstring(state, value);
    lua_setglobal(state, name);

    return CSC_SUCCESS;
}

int csc_lua_function_exists(csc_lua_t lua, const char *name)
{
    lua_State *state = lua;
    if ( ! state ) {
        return 0;
    }

    lua_getglobal(state, name);
    if (!lua_isfunction(state, 1)) {
        lua_pop(state, 1);  /* Remove the nil value return from get global */
        return 0;
    }
    return 1;
}

int csc_lua_call_arg_0_ret_0(csc_lua_t lua, const char * name )
{
    lua_State *state = lua;
    if ( ! state ) {
        return CSC_ERROR;
    }

    /* Get function  */
    lua_getglobal(state, name);
    if (!lua_isfunction(state, 1)) {
        lua_pop(state, 1);  /* Remove the nil value return from get global */
        return -1;
    }
    lua_pcall(state, 0, 0,0);

    return CSC_SUCCESS;
}

int csc_lua_call_arg_0_ret_i(csc_lua_t lua, const char * name , int *r1)
{
    lua_State *state = lua;
    if ( ! state ) {
        return CSC_ERROR;
    }

    /* Get function  */
    lua_getglobal(state, name);
    if (!lua_isfunction(state, 1)) {
        lua_pop(state, 1);  /* Remove the nil value return from get global */
        return -1;
    }
    lua_pcall(state, 0, 1,0);
    *r1 = lua_tointeger(state,-1);
    lua_pop(state, 1);
    return CSC_SUCCESS;
}

int csc_lua_call_arg_0_ret_ii(csc_lua_t lua, const char * name , int *r1, int *r2)
{
    lua_State *state = lua;
    if ( ! state ) {
        return CSC_ERROR;
    }

    /* Get function  */
    lua_getglobal(state, name);
    if (!lua_isfunction(state, 1)) {
        lua_pop(state, 1);  /* Remove the nil value return from get global */
        return -1;
    }
    lua_pcall(state, 0, 2,0);
    *r1 = lua_tointeger(state,-2);
    *r2 = lua_tointeger(state,-1);
    lua_pop(state, 2);
    return CSC_SUCCESS;
}

int csc_lua_call_arg_i_ret_i(csc_lua_t lua, const char * name , int i1, int *r1)
{
    lua_State *state = lua;
    if ( ! state ) {
        return CSC_ERROR;
    }

    /* Get function  */
    lua_getglobal(state, name);
    if (!lua_isfunction(state, 1)) {
        lua_pop(state, 1);  /* Remove the nil value return from get global */
        return -1;
    }
    lua_pushinteger(state, i1);

    lua_pcall(state, 1, 1,0);
    *r1 = lua_tointeger(state,-1);
    lua_pop(state, 1);


    return CSC_SUCCESS;
}


int csc_lua_call_arg_ii_ret_i(csc_lua_t lua, const char * name , int i1, int i2, int *r1)
{
    lua_State *state = lua;
    if ( ! state ) {
        return CSC_ERROR;
    }

    /* Get function  */
    lua_getglobal(state, name);
    if (!lua_isfunction(state, 1)) {
        lua_pop(state, 1);  /* Remove the nil value return from get global */
        return -1;
    }

    lua_pushinteger(state, i1);
    lua_pushinteger(state, i2);

    lua_pcall(state, 2, 1,0);
    *r1 = lua_tointeger(state,-1);
    lua_pop(state, 1);

    return CSC_SUCCESS;
}

int csc_lua_call_arg_iii_ret_i(csc_lua_t lua, const char * name , int i1, int i2, int i3, int *r1)
{
    lua_State *state = lua;
    if ( ! state ) {
        return CSC_ERROR;
    }

    /* Get function  */
    lua_getglobal(state, name);
    if (!lua_isfunction(state, 1)) {
        lua_pop(state, 1);  /* Remove the nil value return from get global */
        return -1;
    }

    lua_pushinteger(state, i1);
    lua_pushinteger(state, i2);
    lua_pushinteger(state, i3);

    lua_pcall(state, 3, 1,0);
    *r1 = lua_tointeger(state,-1);
    lua_pop(state, 1);


    return CSC_SUCCESS;
}

int csc_lua_call_arg_sii_ret_i(csc_lua_t lua, const char * name , const char * i1, int i2, int i3, int *r1)
{
    lua_State *state = lua;
    if ( ! state ) {
        return CSC_ERROR;
    }

    /* Get function  */
    lua_getglobal(state, name);
    if (!lua_isfunction(state, 1)) {
        lua_pop(state, 1);  /* Remove the nil value return from get global */
        return -1;
    }

    lua_pushstring(state, i1);
    lua_pushinteger(state, i2);
    lua_pushinteger(state, i3);

    lua_pcall(state, 3, 1,0);
    *r1 = lua_tointeger(state,-1);
    lua_pop(state, 1);


    return CSC_SUCCESS;
}


