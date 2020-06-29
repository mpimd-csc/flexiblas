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

#ifndef CSCUTILS_LUA_H
#define CSCUTILS_LUA_H

#include "cscutils/cscutils_config.h"

#ifdef __cplusplus
extern "C" {
#endif

    /**
     * @file libcscutils/include/cscutils/lua.h
     * @defgroup lua LUA Interface: Interface to the embedded scripting language LUA
     *
     * This part of the libary contains an interface to the LUA programming language.
     * LUA is mostly used as scripting language embedded into other applicaitons. We
     * use is for example to change tuning parameters for numerical algorithms
     * without recompiling the applicagtions.
     *
     * @addtogroup lua
     * @{
     */

    typedef void* csc_lua_t;

    /**
     * \brief Initialize the LUA interface.
     * \return A pointer to the LUA interpreter on success, NULL otherwise.
     *
     * The csc_lua_init function initializes the LUA interface and loads the
     * LUA standard libraries into it.
     */
    csc_lua_t csc_lua_init();

    /**
     * \brief Destroys a LUA interface.
     * \param[in] lua   The LUA interpreter to destroy.
     *
     * The csc_lua_finalize function terminates the LUA interface and cleans up the
     * internal data structures.
     */
    void csc_lua_finalize(csc_lua_t lua);

    /**
     * \brief Load a LUA file into the internal buffer.
     * \param[inout] lua    The LUA interpreter to use.
     * \param[in] filename  Filename of the LUA file.
     * \return 0 on success, -1 otherwise
     *
     * The csc_lua_loadfile function loads a LUA interpreter. The
     * file can either be a human-readable one or a file precompiled with luac.
     * The file is not executed yet. This is done by \ref csc_lua_run.
     * If multiple loads should be performed, it needs a
     * \ref csc_lua_run after each load operations.
     *
     * \sa csc_lua_run
     * \sa csc_lua_loadstring
     *
     */
    int csc_lua_loadfile(csc_lua_t lua, const char * filename);


    /**
     * \brief Load a LUA script from a string.
     * \param[inout]    lua   The LUA interpreter to use.
     * \param[in]       code  The LUA script as string.
     * \return 0 on success, -1 otherwise
     *
     * The csc_lua_loadstring function loads a LUA script from a string. The
     * script is not executed immediately. This requires a separate call to
     * \ref csc_lua_run. If multiple loads should be performed, it needs a
     * \ref csc_lua_run after each load operations.
     *
     * \sa csc_lua_run
     * \sa csc_lua_loadfile
     *
     */
    int csc_lua_loadstring(csc_lua_t lua, const char * code );

    /**
     * \brief Run the LUA scripts loaded into an interpreter
     * \param[inout] lua    The LUA interpreter to run.
     * \return 0 on success or -1 otherwise.
     *
     * The csc_lua_run function executes the LUA scripts loaded into
     * the interpreter.
     *
     * \ref csc_lua_loadfile
     */
    int csc_lua_run(csc_lua_t lua);


    /**
     * \brief Set a global integer variable.
     * \param[inout]    lua     The LUA interpreter to work with
     * \param[in]       name    Name of the global variable.
     * \param[in]       value   Value of the variable.
     * \return 0 on success, -1 otherwise.
     *
     * The csc_lua_global_int function sets a global variable in the
     * LUA interpreter. This can be done as well before the script is
     * executed.
     *
     */
    int csc_lua_global_int(csc_lua_t lua, const char *name, int value);

    /**
     * \brief Set a global floating point variable.
     * \param[inout]    lua     The LUA interpreter to work with
     * \param[in]       name    Name of the global variable.
     * \param[in]       value   Value of the variable.
     * \return 0 on success, -1 otherwise.
     *
     * The csc_lua_global_double function sets a global variable in the
     * LUA interpreter. This can be done as well before the script is
     * executed.
     *
     */
    int csc_lua_global_double(csc_lua_t lua, const char *name, double value);

    /**
     * \brief Set a global string variable.
     * \param[inout]    lua     The LUA interpreter to work with
     * \param[in]       name    Name of the global variable.
     * \param[in]       value   Value of the variable.
     * \return 0 on success, -1 otherwise.
     *
     * The csc_lua_global_string function sets a global variable in the
     * LUA interpreter. This can be done as well before the script is
     * executed.
     *
     */
    int csc_lua_global_string(csc_lua_t lua, const char *name, const char *value);

    /**
     * \brief Check if a LUA function exists
     * \param[inout]    lua     The LUA interpreter to use.
     * \param[in]       name    Name of the function
     * \return 1 if the function exists, 0 otherwise
     *
     * The csc_lua_function_exists function checks if a function exists in the
     * given instance of the LUA interpreter.
     *
     */
    int csc_lua_function_exists(csc_lua_t lua, const char *name);

    /**
     * \brief Call a LUA function with 0 Inputs and 0 Outputs
     * \param[inout]       lua     The LUA interpreter to use
     * \param[in]       name    Name of the function to call.
     * \return 0 on success, -1 otherwise
     *
     * The csc_lua_call_arg_0_ret_0 calls a LUA function with zero inputs
     * and zero outputs.
     *
     */
    int csc_lua_call_arg_0_ret_0(csc_lua_t lua, const char * name );

    /**
     * \brief Call a LUA function with 0 Inputs and 1 Outputs
     * \param[inout]       lua     The LUA interpreter to use
     * \param[in]       name    Name of the function to call.
     * \param[out]      r1      Integer return value.
     * \return 0 on success, -1 otherwise
     *
     * The csc_lua_call_arg_0_ret_i calls a LUA function with one
     * integer return value and zero inputs.
     *
     */
    int csc_lua_call_arg_0_ret_i(csc_lua_t lua, const char * name , int *r1);

    /**
     * \brief Call a LUA function with 0 Inputs and 2 Outputs
     * \param[inout]       lua     The LUA interpreter to use
     * \param[in]       name    Name of the function to call.
     * \param[out]      r1      First integer return value.
     * \param[out]      r2      Second integer return value.
     * \return 0 on success, -1 otherwise
     *
     * The csc_lua_call_arg_0_ret_i calls a LUA function with two
     * integer return value and zero inputs.
     *
     */
    int csc_lua_call_arg_0_ret_ii(csc_lua_t lua, const char * name , int *r1, int *r2);

    /**
     * \brief Call a LUA function with 1 Inputs and 1 Outputs
     * \param[inout]       lua     The LUA interpreter to use
     * \param[in]       name    Name of the function to call.
     * \param[in]       i1      First integer input.
     * \param[out]      r1      Integer return value.
     * \return 0 on success, -1 otherwise
     *
     * The csc_lua_call_arg_i_ret_i calls a LUA function with one
     * integer return value and one integer intput.
     *
     */
    int csc_lua_call_arg_i_ret_i(csc_lua_t lua, const char * name , int i1, int *r1);

    /**
     * \brief Call a LUA function with 2 Inputs and 1 Outputs
     * \param[inout]       lua     The LUA interpreter to use
     * \param[in]       name    Name of the function to call.
     * \param[in]       i1      First integer input.
     * \param[in]       i2      Second integer input.
     * \param[out]      r1      Integer return value.
     * \return 0 on success, -1 otherwise
     *
     * The csc_lua_call_arg_ii_ret_i calls a LUA function with one
     * integer return value and two integer intputs.
     *
     */
    int csc_lua_call_arg_ii_ret_i(csc_lua_t lua, const char * name , int i1, int i2, int *r1);

    /**
     * \brief Call a LUA function with 3 Inputs and 1 Outputs
     * \param[inout]       lua     The LUA interpreter to use
     * \param[in]       name    Name of the function to call
     * \param[in]       i1      First integer input
     * \param[in]       i2      Second integer input
     * \param[in]       i3      Third integer input
     * \param[out]      r1      Integer return value
     * \return 0 on success, -1 otherwise
     *
     * The csc_lua_call_arg_iii_ret_i calls a LUA function with one
     * integer return value and three integer intputs.
     *
     */
    int csc_lua_call_arg_iii_ret_i(csc_lua_t lua, const char * name , int i1, int i2, int i3, int *r1);

    /**
     * \brief Call a LUA function with 3 Inputs and 1 Outputs
     * \param[inout]       lua     The LUA interpreter to use
     * \param[in]       name    Name of the function to call
     * \param[in]       i1      First string input
     * \param[in]       i2      Second integer input
     * \param[in]       i3      Third integer input
     * \param[out]      r1      Integer return value
     * \return 0 on success, -1 otherwise
     *
     * The csc_lua_call_arg_sii_ret_i calls a LUA function with one
     * integer return value, a string input and two integer intputs.
     *
     */
    int csc_lua_call_arg_sii_ret_i(csc_lua_t lua, const char * name , const char * i1, int i2, int i3, int *r1);

    /** @} */

#ifdef __cplusplus
};
#endif
#endif /* end of include guard: CSCUTILS_LUA_H */

