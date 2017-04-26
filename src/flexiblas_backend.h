/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2013, 2014, 2015
 */

#ifndef __FLEXIBLAS_BACKEND_H
#define __FLEXIBLAS_BACKEND_H

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

// #include "flexiblas_structures.h"
#include "flexiblas_real_calls.h"
#include "flexiblas_real_extblas_calls.h"

#define FLEXIBLAS_INIT_FUNCTION_NAME "__flexiblas_initialize"
#define FLEXIBLAS_EXIT_FUNCTION_NAME "__flexiblas_finalize" 
#define FLEXIBLAS_INFO_FUNCTION_NAME "__flexiblas_info"

#define FLEXIBLAS_LAZY_BINDING int32_t flexiblas_ld_lazy = 1; 
#define FLEXIBLAS_NOW_BINDING int32_t flexiblas_ld_lazy = 0; 
#define FLEXIBLAS_GLOBAL_BINDING int32_t flexiblas_ld_global = 1;                                                                               
#define FLEXIBLAS_LOCAL_BINDING int32_t flexiblas_ld_global = 0;                                                                               
#define FLEXIBLAS_DEEP_BINDING int32_t flexiblas_ld_deep = 1;                                                                               


#ifdef __cplusplus
#define FLEXIBLAS_INFO_FUNCTION(info) extern "C" void __flexiblas_info(flexiblas_info_t *info) 
#define FLEXIBLAS_INIT_FUNCTION extern "C" void __flexiblas_initialize
#define FLEXIBLAS_EXIT_FUNCTION extern "C" void __flexiblas_finalize

#else 
#define FLEXIBLAS_INFO_FUNCTION(info) void __flexiblas_info(flexiblas_info_t *info) 
#define FLEXIBLAS_INIT_FUNCTION int __flexiblas_initialize 
#define FLEXIBLAS_EXIT_FUNCTION void __flexiblas_finalize 
#endif

#endif
