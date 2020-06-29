/*
 * LIBCSCUTILS: DEMO Code for LUA
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

#include "cscutils/lua.h"

const char * incode_func();


char code[] =
 "function incode(x)\n"
 " print(\"incode\\n\")\n"
 " print(4*x)\n"
 " return 10\n"
 "end\n";


int main(int argc, char **argv)
{
    csc_lua_t lua;

    if ( argc != 2) {
        printf("Need a script as argument.\n");
        exit(-1);
    }

    lua = csc_lua_init();
    if ( !lua ) {
        printf("init lua failed.\n");
        exit(-1);
    }

    if ( csc_lua_loadstring(lua, code)) {
        printf("Load LUA script from string failed.\n");
    }

    if ( csc_lua_run(lua)) {
        printf("Execution failed.\n");
    }

    if ( csc_lua_loadstring(lua, incode_func())) {
       printf("Load LUA script from CMAKE include failed.\n");
    }

    if ( csc_lua_run(lua)) {
        printf("Execution failed.\n");
    }

    if ( csc_lua_loadfile(lua, argv[1]))
    {
        printf("Load %s failed.\n", argv[1]);
    }



    csc_lua_global_int(lua, "testint", 12345);
    csc_lua_global_double(lua, "testnumber", 1.2345);
    csc_lua_global_string(lua, "teststring", "Hello World");

    if ( csc_lua_run(lua)) {
        printf("Execution failed.\n");
    }

    csc_lua_call_arg_0_ret_0(lua, "arg0ret0");

    int r;
    csc_lua_call_arg_0_ret_i(lua, "arg0reti", &r);
    printf("returned value: %d\n", r);

    int r2;
    csc_lua_call_arg_0_ret_ii(lua, "arg0retii", &r, &r2);
    printf("returned : %d , %d\n", r, r2);

    csc_lua_call_arg_i_ret_i(lua, "argireti", 123,  &r);
    printf("returned value: %d\n", r);

    csc_lua_call_arg_ii_ret_i(lua, "argiireti", 123, 10, &r);
    printf("returned value: %d\n", r);

    csc_lua_call_arg_iii_ret_i(lua, "argiiireti", 123, 10, 5,  &r);
    printf("returned value: %d\n", r);

    csc_lua_call_arg_sii_ret_i(lua, "argsiireti", "test", 123, 10, &r);
    printf("returned value: %d\n", r);

    csc_lua_call_arg_i_ret_i(lua, "incode", 20, &r);
    printf("returned from incode : %d\n", r );

    csc_lua_call_arg_i_ret_i(lua, "inlinecode", 20, &r);
    printf("returned from inlinecode : %d\n", r );

    csc_lua_finalize(lua);
    return 0;
}
