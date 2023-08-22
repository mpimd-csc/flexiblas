/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Linking FlexiBLAS statically or dynamically with other modules is making a
 * combined work based on FlexiBLAS. Thus, the terms and conditions of the GNU
 * General Public License cover the whole combination.
 *
 * As a special exception, the copyright holders of FlexiBLAS give you permission
 * to combine FlexiBLAS program with free software programs or libraries that are
 * released under the GNU LGPL and with independent modules that communicate with
 * FlexiBLAS solely through the BLAS/LAPACK interface as provided by the
 * BLAS/LAPACK reference implementation. You may copy and distribute such a system
 * following the terms of the GNU GPL for FlexiBLAS and the licenses of the other
 * code concerned, provided that you include the source code of that other code
 * when and as the GNU GPL requires distribution of source code and provided that
 * you do not modify the BLAS/LAPACK interface.
 *
 * Note that people who make modified versions of FlexiBLAS are not obligated to
 * grant this special exception for their modified versions; it is their choice
 * whether to do so. The GNU General Public License gives permission to release a
 * modified version without this exception; this exception also makes it possible
 * to release a modified version which carries forward this exception. If you
 * modify the BLAS/LAPACK interface, this exception does not apply to your
 * modified version of FlexiBLAS, and you must remove this exception when you
 * distribute your modified version.
 *
 * This exception is an additional permission under section 7 of the GNU General
 * Public License, version 3 (“GPLv3”)
 *
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2013-2023
 */



#ifndef __FLEXIBLAS_BACKEND_H
#define __FLEXIBLAS_BACKEND_H

#include <string.h>
#include "flexiblas_mgmt.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef Int
#ifndef INTEGER8
#define Int 	int
#define blasint int
#else
#include <stdint.h>
#define Int 	int64_t
#define blasint int64_t
#endif
#endif

#ifndef blasint
#ifdef INTEGER8
#define blasint int64_t
#else
#define blasint int
#endif
#endif

typedef struct _flexiblas_info_t {
	int flexiblas_integer_size;
	int backend_integer_size;
	int intel_interface;
	int post_init;
} flexiblas_info_t;

typedef struct _flexiblas_hook_register_t {
    char * name;
    char * cfg_name;
    char * desc;
    char * authors;
} flexiblas_hook_register_t;

typedef enum {
    FLEXIBLAS_OPTIONS_INT = 0,
    FLEXIBLAS_OPTIONS_STRING = 1,
    FLEXIBLAS_OPTIONS_FLOAT = 2
} flexiblas_option_type_t;

typedef struct _flexiblas_option_t {
    char *name;
    char *desc;
    flexiblas_option_type_t type;
    char *def;
} flexiblas_option_t;

/* Routines from FlexiBLAS public API */
extern int flexiblas_verbosity(void);
extern flexiblas_mgmt_t * flexiblas_mgmt(void);
extern void flexiblas_print_error(const char *prefix, const char *path, const int line, const char *fmt, ... );
extern void flexiblas_print_warning(const char *prefix, const char *fmt, ... );
extern void flexiblas_print_info(const char *prefix, const char *fmt, ... );
extern double flexiblas_wtime(void);


#define FLEXIBLAS_INIT_FUNCTION_NAME "__flexiblas_initialize"
#define FLEXIBLAS_EXIT_FUNCTION_NAME "__flexiblas_finalize"
#define FLEXIBLAS_INFO_FUNCTION_NAME "__flexiblas_info"

#define FLEXIBLAS_HOOK_INIT_FUNCTION_NAME "__flexiblas_hook_initialize"
#define FLEXIBLAS_HOOK_EXIT_FUNCTION_NAME "__flexiblas_hook_finalize"


#define FLEXIBLAS_LAZY_BINDING int32_t flexiblas_ld_lazy = 1;
#define FLEXIBLAS_NOW_BINDING int32_t flexiblas_ld_lazy = 0;
#define FLEXIBLAS_GLOBAL_BINDING int32_t flexiblas_ld_global = 1;
#define FLEXIBLAS_LOCAL_BINDING int32_t flexiblas_ld_global = 0;
#define FLEXIBLAS_DEEP_BINDING int32_t flexiblas_ld_deep = 1;


#ifdef __cplusplus
#define FLEXIBLAS_INFO_FUNCTION(info) extern "C" void __flexiblas_info(flexiblas_info_t *info)
#define FLEXIBLAS_INIT_FUNCTION extern "C" void __flexiblas_initialize
#define FLEXIBLAS_EXIT_FUNCTION extern "C" void __flexiblas_finalize
#define FLEXIBLAS_HOOK_INIT_FUNCTION extern "C" void __flexiblas_hook_initialize
#define FLEXIBLAS_HOOK_EXIT_FUNCTION extern "C" void __flexiblas_hook_finalize

#else
#define FLEXIBLAS_INFO_FUNCTION(info) void __flexiblas_info(flexiblas_info_t *info)
#define FLEXIBLAS_INIT_FUNCTION int __flexiblas_initialize
#define FLEXIBLAS_EXIT_FUNCTION void __flexiblas_finalize
#define FLEXIBLAS_HOOK_INIT_FUNCTION void __flexiblas_hook_initialize
#define FLEXIBLAS_HOOK_EXIT_FUNCTION void __flexiblas_hook_finalize

#define FLEXIBLAS_HOOK_REGISTER(hookname, secname, nsp, desc, authors) \
    flexiblas_hook_register_t flexiblas_register = { hookname, secname, desc, authors }; \
    int flexiblas_hook_ ## nsp ##_get_int (char *option) { \
        flexiblas_mgmt_t * config = flexiblas_mgmt(); \
        int val = 0;\
        int ret = flexiblas_mgmt_hook_option_get_int(config, secname, option, &val);\
        if (ret != 0) {\
            int i = 0;\
            while(flexiblas_options[i].name != NULL && strcmp(flexiblas_options[i].name, option) != 0) i++;\
            if (flexiblas_options[i].name == NULL) { fprintf(stderr, "%s: Options %s not found in options list.\n", hookname, option); abort(); }\
            return atoi(flexiblas_options[i].def);\
        }\
        return val; \
    } \
    char *flexiblas_hook_ ## nsp ##_get_string (char *option) { \
        flexiblas_mgmt_t * config = flexiblas_mgmt(); \
        char val[FLEXIBLAS_MGMT_MAX_BUFFER_LEN]; \
        int ret = flexiblas_mgmt_hook_option_get_string(config, secname, option, val);\
        if (ret != 0) {\
            int i = 0;\
            while(flexiblas_options[i].name != NULL && strcmp(flexiblas_options[i].name, option) != 0) i++;\
            if (flexiblas_options[i].name == NULL) { fprintf(stderr, "%s: Options %s not found in options list.\n", hookname, option); abort(); }\
            return strdup(flexiblas_options[i].def);\
        }\
        return strdup(val); \
    } \
    double flexiblas_hook_ ## nsp ##_get_float (char *option) { \
        flexiblas_mgmt_t * config = flexiblas_mgmt(); \
        double val = 0;\
        int ret = flexiblas_mgmt_hook_option_get_float(config, secname, option, &val);\
        if (ret != 0) {\
            int i = 0;\
            while(flexiblas_options[i].name != NULL && strcmp(flexiblas_options[i].name, option) != 0) i++;\
            if (flexiblas_options[i].name == NULL) { fprintf(stderr, "%s: Options %s not found in options list.\n", hookname, option); abort(); }\
            return atof(flexiblas_options[i].def);\
        }\
        return val; \
    }
#define FLEXIBLAS_HOOK_PROTOTYPES(nsp) \
    int    flexiblas_hook_ ## nsp ##_get_int (char *option); \
    char*  flexiblas_hook_ ## nsp ##_get_string (char *option); \
    double flexiblas_hook_ ## nsp ##_get_float (char *option);

#define FLEXIBLAS_HOOK_GET_OPTION_INT(nsp, option) flexiblas_hook_## nsp ## _get_int(option)
#define FLEXIBLAS_HOOK_GET_OPTION_STRING(nsp, option) flexiblas_hook_## nsp ## _get_string(option)
#define FLEXIBLAS_HOOK_GET_OPTION_FLOAT(nsp, option) flexiblas_hook_## nsp ## _get_float(option)




#define FLEXIBLAS_HOOK_OPTIONS(...) flexiblas_option_t flexiblas_options[] = { __VA_ARGS__ };
#define FLEXIBLAS_HOOK_OPTION(name, desc, type, def) { name, desc, type, def }
#define FLEXIBLAS_HOOK_OPTIONS_END {NULL, NULL, FLEXIBLAS_OPTIONS_INT, NULL}

#endif

#ifdef __cplusplus
};
#endif


#endif
