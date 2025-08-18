//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
   This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
   Copyright (C) 2013-2025 Martin Koehler

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
   */



#ifndef FLEXIBLAS_PATHS_H

#define FLEXIBLAS_PATHS_H

#ifdef __cplusplus
extern "C" {
#endif

#include "flexiblas_config.h"
#include "flexiblas_mgmt.h"


    HIDDEN extern int __flexiblas_count_additional_paths;
    HIDDEN extern char **  __flexiblas_additional_paths;


    HIDDEN void __flexiblas_add_path(const char * path );
    HIDDEN void __flexiblas_free_paths(void);
    HIDDEN void __flexiblas_init_default_paths(void);
    HIDDEN void __flexiblas_add_path_from_environment(void);
    HIDDEN void __flexiblas_add_path_from_config( flexiblas_mgmt_t * config, flexiblas_mgmt_location_t loc);
    HIDDEN char * __flexiblas_get_library_location(void);
    HIDDEN void __flexiblas_get_global_rc_path(char * container, int max_buffer_size,  char const * suffix);

#ifdef __cplusplus
};
#endif



#endif /* end of include guard: FLEXIBLAS_PATHS_H */

