//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
   This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
   Copyright (C) 2013-2024 Martin Koehler

   This program is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation, either version 3 of the License, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along
   with this program. If not, see <https://www.gnu.org/licenses/>.
   */




#ifndef FLEXIBLAS_HOOKS_H

#define FLEXIBLAS_HOOKS_H

#ifdef __cplusplus
extern "C" {
#endif

#include "flexiblas_config.h"

    HIDDEN void __flexiblas_list_hooks(void);
    HIDDEN void __flexiblas_add_hooks(void);
    HIDDEN void __flexiblas_exit_hook(void);
    HIDDEN int __flexiblas_hook_exists(char *name);
    HIDDEN char * __flexiblas_hook_sofile(char *name);
    HIDDEN char *  __flexiblas_hook_add_from_file(char *path);
    HIDDEN void __flexiblas_hook_list(int *nelem, char ***list, char ***list2);

#ifdef __cplusplus
};
#endif

#endif /* end of include guard: FLEXIBLAS_HOOKS_H */

