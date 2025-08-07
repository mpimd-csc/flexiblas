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



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#ifndef __WIN32__
#include <dlfcn.h>
#else
#include <windows.h>
#define RTLD_LAZY 0
#define RTLD_LOCAL 0
#endif
#include <ctype.h>

#include "cscutils/map.h"

#include "flexiblas_backend.h"
#include "hooks.h"
#include "paths.h"
#include "helper.h"

static csc_map_t * hook_map = NULL;


HIDDEN void __flexiblas_list_hooks(void)
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

#if defined(__WIN32__)
            snprintf(curfn, curfnl, "%s\\%s", curpath, dir_entry->d_name);
#else
            snprintf(curfn, curfnl, "%s/%s", curpath, dir_entry->d_name);
#endif
            memset(&st, 0, sizeof(struct stat));
            if ( stat(curfn, &st)) continue;
            if ( ! ( S_ISREG(st.st_mode))) continue;

            printf("%s\n", curfn);

            handle  = __flexiblas_dlopen(curfn, RTLD_LAZY | RTLD_LOCAL , NULL);
            if ( !handle) continue;

#ifdef __WIN32__
            reg = (flexiblas_hook_register_t *) GetProcAddress(handle, "flexiblas_register");
            if ( !reg ) {
                DPRINTF(0, "%s is not a hook\n");
                FreeLibrary(handle);
                continue;
            }
#else
            reg = dlsym(handle,"flexiblas_register");
            if ( !reg ) {
                DPRINTF(0, "%s is not a hook\n");
                dlclose(handle);
                continue;
            }
#endif

            printf("Hook %s\n", curfn);
            printf("-> Name:     %s\n", reg->name);
            printf("-> Cfg-Name: %s\n", reg->cfg_name);
            printf("-> Descr:    %s\n", reg->desc);
            printf("-> Authors:  %s\n", reg->authors);

#ifdef __WIN32__
            opts = (flexiblas_option_t *) GetProcAddress(handle, "flexiblas_options");
            if (!opts) {
                FreeLibrary(handle);
                continue;
            }
#else
            opts = dlsym(handle, "flexiblas_options");
            if (!opts) {
                dlclose(handle);
                continue;
            }
#endif

            while ( opts->name != NULL) {
                printf("opts->name: %s\n", opts->name);
                opts = opts + 1;

            }


#ifdef __WIN32__
            FreeLibrary(handle);
#else
            dlclose(handle);
#endif
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


HIDDEN void __flexiblas_add_hooks(void)
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

#if defined(__WIN32__)
            snprintf(curfn, curfnl, "%s\\%s", curpath, dir_entry->d_name);
#else
            snprintf(curfn, curfnl, "%s/%s", curpath, dir_entry->d_name);
#endif
            memset(&st, 0, sizeof(struct stat));
            if ( stat(curfn, &st)) continue;
            if ( ! ( S_ISREG(st.st_mode))) continue;

            handle  = __flexiblas_dlopen(curfn, RTLD_LAZY | RTLD_LOCAL , NULL);
            if ( !handle) continue;


#ifdef __WIN32__
            reg = (flexiblas_hook_register_t *) GetProcAddress(handle, "flexiblas_register");
            if ( !reg ) {
                DPRINTF(0, "%s is not a hook\n");
                FreeLibrary(handle);
                continue;
            }
#else
            reg = dlsym(handle,"flexiblas_register");
            if ( !reg ) {
                DPRINTF(1, "%s is not a hook\n", dir_entry->d_name);
                dlclose(handle);
                continue;
            }
#endif

            DPRINTF(1, "Hook \"%s/%s\" found in %s\n", reg->name, reg->cfg_name, curfn);
            char * insert_str = __struppercase(strdup(reg->cfg_name));
            csc_map_insert(hook_map, insert_str, strdup(curfn));

            free(curfn);
#ifdef __WIN32__
            FreeLibrary(handle);
#else
            dlclose(handle);
#endif
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

#ifdef __WIN32__
    reg = (flexiblas_hook_register_t *) GetProcAddress(handle, "flexiblas_register");
#else
    reg = dlsym(handle, "flexiblas_register");
#endif
    if ( !reg ) return NULL;

    ret = strdup(reg->cfg_name);

    if ( csc_map_has_key(hook_map, reg->cfg_name)) {
        DPRINTF_WARN(0, "Hook %s from %s already exists in the configuration.\n", reg->cfg_name, path);
        DPRINTF_WARN(0, "The previously found hook (%s) will be replaced.\n", csc_map_get(hook_map, reg->cfg_name));
        csc_map_replace(hook_map, reg->cfg_name, strdup(path));
    } else {
        csc_map_insert(hook_map, reg->cfg_name, strdup(path));
    }

#ifdef __WIN32__
    FreeLibrary(handle);
#else
    dlclose(handle);
#endif
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

HIDDEN void __flexiblas_exit_hook(void)
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

