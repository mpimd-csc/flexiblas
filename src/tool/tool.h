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
 * Copyright (C) Martin Koehler, 2013-2022
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
