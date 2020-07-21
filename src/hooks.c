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
 * Copyright (C) Martin Koehler, 2013-2020
 */



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <dlfcn.h>
#include <ctype.h>

#include "cscutils/map.h"

#include "flexiblas_backend.h"
#include "hooks.h"
#include "paths.h"
#include "helper.h"

static csc_map_t * hook_map = NULL;


HIDDEN void __flexiblas_list_hooks()
{

    int i ;
    char *curpath;
    DIR* folder;
    struct dirent *dir_entry;
    size_t len = strlen("libflexiblas_hook");
    struct stat st;
    char *curfn;
    size_t curfnl;
    void * handle;
    flexiblas_hook_register_t *reg;
    flexiblas_option_t *opts;

    for (i = 0; i < __flexiblas_count_additional_paths; i++) {
        curpath = __flexiblas_additional_paths[i];

        DPRINTF(0,"Search in: %s\n", curpath);
        folder = opendir(curpath);
        if (!folder) continue;

        while ((dir_entry = readdir(folder)) != NULL) {
            if ( strncmp(dir_entry->d_name, "..", 2) == 0 ) continue;
            if ( strncmp(dir_entry->d_name, ".", 1) == 0 ) continue;
            if ( strncmp(dir_entry->d_name, "libflexiblas_hook", len) != 0 ) continue;

            curfnl  = (strlen(curpath) + strlen(dir_entry->d_name) + 5);
            curfn = malloc(sizeof(char) * curfnl);

            snprintf(curfn, curfnl, "%s/%s", curpath, dir_entry->d_name);
            memset(&st, 0, sizeof(struct stat));
            if ( stat(curfn, &st)) continue;
            if ( ! ( S_ISREG(st.st_mode))) continue;

            printf("%s\n", curfn);

            handle  = __flexiblas_dlopen(curfn, RTLD_LAZY | RTLD_LOCAL , NULL);
            if ( !handle) continue;

            reg = dlsym(handle,"flexiblas_register");
            if ( !reg ) {
                DPRINTF(0, "%s is not a hook\n");
                dlclose(handle);
                continue;
            }

            printf("Hook %s\n", curfn);
            printf("-> Name:     %s\n", reg->name);
            printf("-> Cfg-Name: %s\n", reg->cfg_name);
            printf("-> Descr:    %s\n", reg->desc);
            printf("-> Authors:  %s\n", reg->authors);

            opts = dlsym(handle, "flexiblas_options");
            if ( !opts) {
                dlclose(handle);
                continue;
            }

            while ( opts->name != NULL) {
                printf("opts->name: %s\n", opts->name);
                opts = opts + 1;

            }


            dlclose(handle);
        }

        closedir(folder);
    }

}

static char *__struppercase(char *str) {
	char *ret = str;
	if ( str == NULL ) return NULL;
	while (*str != '\0') {
		*str = toupper(*str);
		str++;
	}
	return ret;
}


HIDDEN void __flexiblas_add_hooks()
{

    int i ;
    char *curpath;
    DIR* folder;
    struct dirent *dir_entry;
    size_t len = strlen("libflexiblas_hook");
    struct stat st;
    char *curfn;
    size_t curfnl;
    void * handle;
    flexiblas_hook_register_t *reg;

    hook_map = csc_map_new_string_key(257,free);

    for (i = 0; i < __flexiblas_count_additional_paths; i++) {
        curpath = __flexiblas_additional_paths[i];

        folder = opendir(curpath);
        if (!folder) continue;

        while ((dir_entry = readdir(folder)) != NULL) {
            if ( strncmp(dir_entry->d_name, "..", 2) == 0 ) continue;
            if ( strncmp(dir_entry->d_name, ".", 1) == 0 ) continue;
            if ( strncmp(dir_entry->d_name, "libflexiblas_hook", len) != 0 ) continue;

            curfnl  = (strlen(curpath) + strlen(dir_entry->d_name) + 5);
            curfn = malloc(sizeof(char) * curfnl);

            snprintf(curfn, curfnl, "%s/%s", curpath, dir_entry->d_name);
            memset(&st, 0, sizeof(struct stat));
            if ( stat(curfn, &st)) continue;
            if ( ! ( S_ISREG(st.st_mode))) continue;

            handle  = __flexiblas_dlopen(curfn, RTLD_LAZY | RTLD_LOCAL , NULL);
            if ( !handle) continue;


            reg = dlsym(handle,"flexiblas_register");
            if ( !reg ) {
                DPRINTF(1, "%s is not a hook\n", dir_entry->d_name);
                dlclose(handle);
                continue;
            }

            DPRINTF(1, "Hook \"%s/%s\" found in %s\n", reg->name, reg->cfg_name, curfn);
            char * insert_str = __struppercase(strdup(reg->cfg_name));
            csc_map_insert(hook_map, insert_str, strdup(curfn));

            free(curfn);
            dlclose(handle);
        }

        closedir(folder);
    }

}

HIDDEN char *  __flexiblas_hook_add_from_file(char *path)
{
    void * handle;
    flexiblas_hook_register_t *reg;
    char *ret;

    handle  = __flexiblas_dlopen(path, RTLD_LAZY | RTLD_LOCAL , NULL);
    if ( !handle) return NULL;

    reg = dlsym(handle, "flexiblas_register");
    if ( !reg ) return NULL;

    ret = strdup(reg->cfg_name);

    if ( csc_map_has_key(hook_map, reg->cfg_name)) {
        DPRINTF_WARN(0, "Hook %s from %s already exists in the configuration.\n", reg->cfg_name, path);
        DPRINTF_WARN(0, "The previously found hook (%s) will be replaced.\n", csc_map_get(hook_map, reg->cfg_name));
        csc_map_replace(hook_map, reg->cfg_name, strdup(path));
    } else {
        csc_map_insert(hook_map, reg->cfg_name, strdup(path));
    }

    dlclose(handle);
    return ret;
}

HIDDEN int __flexiblas_hook_exists(char *name)
{
    char *upper_name = __struppercase(strdup(name));
    int ret = csc_map_has_key(hook_map, (void *) upper_name);
    free(upper_name);
    return ret;
}

HIDDEN char * __flexiblas_hook_sofile(char *name)
{
    if (__flexiblas_hook_exists(name)) {
        return csc_map_get(hook_map, (void *) name);
    } else {
        return NULL;
    }
}

HIDDEN void __flexiblas_exit_hook( )
{
    csc_map_free(hook_map);
}

HIDDEN void __flexiblas_hook_list(int *nelem, char ***list, char ***list2)
{
    int len;
    char **ilist;
    char **ilist2;
    void *iter = NULL;
    char *key;
    int i = 0;

    len = csc_map_len(hook_map);
    ilist = (char * *) malloc(sizeof(char *) * (len));
    if ( list2 != NULL) {
        ilist2 = (char * *) malloc(sizeof(char *) * (len));
    }

    while ( (key = csc_map_iterate_key(hook_map, &iter)) != NULL) {
        ilist[i] = strdup(key);
        if (ilist2 !=NULL) {
            ilist2[i] = strdup(csc_map_get(hook_map, key));
        }
        i++;
    }

    *nelem = len;
    *list = ilist;
    if ( list2 ) * list2=ilist2;
    return;
}

