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





#ifndef FLEXIBLAS_TOOL_H

#define FLEXIBLAS_TOOL_H

#ifdef __cplusplus
extern "C" {
#endif

    extern int pipe_output;

    /*  Hook handling  */
    int disable_all_hooks(flexiblas_mgmt_location_t loc);
    int disable_hook(flexiblas_mgmt_location_t loc, const char *name);
    int enable_hook(flexiblas_mgmt_location_t loc, char *name);
    int  show_hook(char *name);
    int list_all_hooks(void);
    int hook_option_set(flexiblas_mgmt_location_t loc, char *hookname, char *option, char *value);
    int hook_option_unset(flexiblas_mgmt_location_t loc, char *hookname, char *option);
    int list_enabled_hooks(void);
    int list_active_hooks(void);

    /* BLAS handling   */
    int remove_blas (flexiblas_mgmt_location_t loc, char *name);
    int add_blas (flexiblas_mgmt_location_t loc, char *name, char *blas, char *comment);
    int set_blas(flexiblas_mgmt_location_t loc, char* name);
    int list_all_blas(void);
    int print_blas(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc, char *where );

#ifdef __cplusplus
};
#endif

#endif /* end of include guard: FLEXIBLAS_TOOL_H */
