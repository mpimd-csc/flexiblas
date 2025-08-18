/*
 * LIBCSCUTILS -- efficient map
 * Copyright (C) Martin Koehler, 2019
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
#ifndef CSC_UTILS_MAP_H
#define CSC_UTILS_MAP_H

#ifdef __cplusplus
extern "C" {
#endif
    #include <stdint.h>
#ifndef __WIN32__
    #include <pthread.h>
#else
    #include "windows_fixes.h"
#endif

    typedef uint64_t (csc_map_hash_func_t)(const void *key, uint64_t size);
    typedef void (csc_map_key_free_t)(void *key);
    typedef void (csc_map_value_free_t)(void *data);
    typedef int (csc_map_key_compare_t) (void *keya, void *keyb);
    typedef void (csc_map_foreach_func_t)(void *key, void*value, void*data);

    typedef struct _csc_map_entry_t {
        void * key;
        void * value;
        uint64_t hash;
        struct _csc_map_entry_t *pprev;
        struct _csc_map_entry_t *pnext;
        struct _csc_map_entry_t *phash;
    } csc_map_entry_t;

    typedef struct _csc_map_t {
        uint64_t size;
        uint64_t len;
        csc_map_hash_func_t *hash_func;
        csc_map_key_free_t  *key_free;
        csc_map_value_free_t *value_free;
        csc_map_key_compare_t *cmp;
        csc_map_entry_t *first, *last;
        csc_map_entry_t **hash_table;
        pthread_mutex_t lock;
    } csc_map_t;

    typedef enum {
        CSC_MAP_SORT_ASC = 0,
        CSC_MAP_SORT_DESC = 1
    } csc_map_sort_direction_t;


    csc_map_t * csc_map_new_expert(uint64_t size, csc_map_hash_func_t *hf, csc_map_key_free_t *kf, csc_map_value_free_t *vf, csc_map_key_compare_t *cmp);
    csc_map_t * csc_map_new_string_key(uint64_t size, csc_map_value_free_t *vf);
    csc_map_t * csc_map_new_int_key(uint64_t size, csc_map_value_free_t * vf);
    uint64_t    csc_map_hash_string(const void *key, uint64_t size);
    uint64_t    csc_map_hash_int(const void *key, uint64_t size);
    void        csc_map_free(csc_map_t *map);
    void        csc_map_free2(csc_map_t *map);
    int         csc_map_insert(csc_map_t *map, void *key, void *val);
    int         csc_map_insert_key(csc_map_t *map, void *key);
    int         csc_map_replace(csc_map_t *map, void * key, void *value);
    int         csc_map_has_key(csc_map_t *map, void *key);
    void *      csc_map_get(csc_map_t *map, void *key);
    int         csc_map_remove(csc_map_t *map, void *key);
    void *      csc_map_iterate(csc_map_t *map, void **ptr);
    void *      csc_map_iterate_key(csc_map_t *map, void **ptr);

    void        csc_map_sort(csc_map_t *map, csc_map_sort_direction_t dir);
    void        csc_map_foreach(csc_map_t * map, csc_map_foreach_func_t * func, void *data);
    size_t      csc_map_len(csc_map_t *map);
    void        csc_map_at(csc_map_t *map, size_t idx, void **key, void **value);

    void        csc_map_dump(FILE * out, csc_map_t *map, int table);
#ifdef __cplusplus
};
#endif

#endif /* end of include guard: CSC_UTILS_MAP_H */

