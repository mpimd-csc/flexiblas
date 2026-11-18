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
extern "C"
{
#endif
#include <stddef.h>
#include <stdint.h>

#ifndef __WIN32__
#include <pthread.h>
#else
#include "windows_fixes.h"
#endif

    /**
     * @brief Hash function type.
     * @param key The key to hash.
     * @param size The size of the hash table.
     * @return The computed hash value.
     */
    typedef uint64_t(csc_map_hash_func_t)(const void* key, uint64_t size);

    /**
     * @brief Key free function type.
     * @param key The key to free.
     */
    typedef void(csc_map_key_free_t)(void* key);

    /**
     * @brief Value free function type.
     * @param data The value to free.
     */
    typedef void(csc_map_value_free_t)(void* data);

    /**
     * @brief Key comparison function type.
     * @param keya First key to compare.
     * @param keyb Second key to compare.
     * @return 0 if equal, non-zero otherwise.
     */
    typedef int(csc_map_key_compare_t)(void* keya, void* keyb);

    /**
     * @brief Foreach function type.
     * @param key The current key.
     * @param value The current value.
     * @param data User-provided data.
     */
    typedef void(csc_map_foreach_func_t)(void* key, void* value, void* data);

    /**
     * @brief Map entry structure.
     */
    typedef struct _csc_map_entry_t
    {
        void* key;                      /**< The key of the entry. */
        void* value;                    /**< The value associated with the key. */
        uint64_t hash;                  /**< The cached hash value of the key. */
        struct _csc_map_entry_t* pprev; /**< Pointer to the previous entry in the list. */
        struct _csc_map_entry_t* pnext; /**< Pointer to the next entry in the list. */
        struct _csc_map_entry_t* phash; /**< Pointer to the next entry in the hash bucket. */
    } csc_map_entry_t;

    /**
     * @brief Map structure.
     */
    typedef struct _csc_map_t
    {
        uint64_t size;                    /**< The size of the hash table. */
        uint64_t len;                     /**< The number of elements in the map. */
        csc_map_hash_func_t* hash_func;   /**< The hash function used by the map. */
        csc_map_key_free_t* key_free;     /**< The function used to free keys. */
        csc_map_value_free_t* value_free; /**< The function used to free values. */
        csc_map_key_compare_t* cmp;       /**< The function used to compare keys. */
        csc_map_entry_t *first, *last;    /**< Pointers to the first and last entries. */
        csc_map_entry_t** hash_table;     /**< The hash table array. */
        pthread_mutex_t lock;             /**< Mutex for thread-safe access. */
    } csc_map_t;

    /**
     * @brief Sort direction for map sorting.
     */
    typedef enum
    {
        CSC_MAP_SORT_ASC = 0, /**< Ascending order. */
        CSC_MAP_SORT_DESC = 1 /**< Descending order. */
    } csc_map_sort_direction_t;

    /**
     * @brief Creates a new map with custom hash, free, and compare functions.
     * @param size The size of the hash table.
     * @param hf The hash function to use.
     * @param kf The function to free keys.
     * @param vf The function to free values.
     * @param cmp The function to compare keys.
     * @return A pointer to the new map, or NULL on failure.
     */
    csc_map_t* csc_map_new_expert(uint64_t size, csc_map_hash_func_t* hf, csc_map_key_free_t* kf, csc_map_value_free_t* vf,
                                  csc_map_key_compare_t* cmp);

    /**
     * @brief Creates a new map with string keys.
     * @param size The size of the hash table.
     * @param vf The function to free values.
     * @return A pointer to the new map, or NULL on failure.
     */
    csc_map_t* csc_map_new_string_key(uint64_t size, csc_map_value_free_t* vf);

    /**
     * @brief Creates a new map with integer keys.
     * @param size The size of the hash table.
     * @param vf The function to free values.
     * @return A pointer to the new map, or NULL on failure.
     */
    csc_map_t* csc_map_new_int_key(uint64_t size, csc_map_value_free_t* vf);

    /**
     * @brief Default hash function for string keys.
     * @param key The string key.
     * @param size The size of the hash table.
     * @return The computed hash value.
     */
    uint64_t csc_map_hash_string(const void* key, uint64_t size);

    /**
     * @brief Default hash function for integer keys.
     * @param key The integer key.
     * @param size The size of the hash table.
     * @return The computed hash value.
     */
    uint64_t csc_map_hash_int(const void* key, uint64_t size);

    /**
     * @brief Frees the map and all its entries, including keys and values.
     * @param map The map to free.
     */
    void csc_map_free(csc_map_t* map);

    /**
     * @brief Frees the map and its entries, including keys, but NOT values.
     * @param map The map to free.
     */
    void csc_map_free2(csc_map_t* map);

    /**
     * @brief Inserts a key-value pair into the map.
     * @param map The map to insert into.
     * @param key The key.
     * @param val The value.
     * @return 0 on success, non-zero on failure (e.g., key already exists or memory failure).
     */
    int csc_map_insert(csc_map_t* map, void* key, void* val);

    /**
     * @brief Inserts a key with a NULL value into the map.
     * @param map The map to insert into.
     * @param key The key.
     * @return 0 on success, non-zero on failure.
     */
    int csc_map_insert_key(csc_map_t* map, void* key);

    /**
     * @brief Replaces the value associated with a key.
     * @param map The map to modify.
     * @param key The key.
     * @param value The new value.
     * @return 0 on success, non-zero if the key was not found.
     */
    int csc_map_replace(csc_map_t* map, void* key, void* value);

    /**
     * @brief Checks if a key exists in the map.
     * @param map The map to search.
     * @param key The key to look for.
     * @return 1 if the key exists, 0 otherwise.
     */
    int csc_map_has_key(csc_map_t* map, void* key);

    /**
     * @brief Retrieves the value associated with a key.
     * @param map The map to search.
     * @param key The key.
     * @return The associated value, or NULL if the key is not found.
     */
    void* csc_map_get(csc_map_t* map, void* key);

    /**
     * @brief Removes a key-value pair from the map.
     * @param map The map to modify.
     * @param key The key to remove.
     * @return 0 on success.
     */
    int csc_map_remove(csc_map_t* map, void* key);

    /**
     * @brief Iterates over the map values.
     * @param map The map to iterate.
     * @param ptr A pointer to the current entry, used for maintaining state between calls.
     *             Should be initialized to NULL.
     * @return The next value in the iteration, or NULL when the end is reached.
     */
    void* csc_map_iterate(csc_map_t* map, void** ptr);

    /**
     * @brief Iterates over the map keys.
     * @param map The map to iterate.
     * @param ptr A pointer to the current entry, used for maintaining state between calls.
     *             Should be initialized to NULL.
     * @return The next key in the iteration, or NULL when the end is reached.
     */
    void* csc_map_iterate_key(csc_map_t* map, void** ptr);

    /**
     * @brief Sorts the map entries.
     * @param map The map to sort.
     * @param dir The sort direction (ASC or DESC).
     */
    void csc_map_sort(csc_map_t* map, csc_map_sort_direction_t dir);

    /**
     * @brief Applies a function to all entries in the map.
     * @param map The map to iterate.
     * @param func The function to apply.
     * @param data User-provided data passed to the function.
     */
    void csc_map_foreach(csc_map_t* map, csc_map_foreach_func_t* func, void* data);

    /**
     * @brief Gets the number of entries in the map.
     * @param map The map.
     * @return The number of entries.
     */
    size_t csc_map_len(csc_map_t* map);

    /**
     * @brief Gets the key and value at a specific index.
     * @param map The map.
     * @param idx The index.
     * @param key Pointer to store the key.
     * @param value Pointer to store the value.
     */
    void csc_map_at(csc_map_t* map, size_t idx, void** key, void** value);

    /**
     * @brief Dumps the map's contents to a file.
     * @param out The output file.
     * @param map The map to dump.
     * @param table If non-zero, dumps the hash table structure; otherwise, dumps the sorted list.
     */
    void csc_map_dump(FILE* out, csc_map_t* map, int table);
#ifdef __cplusplus
};
#endif

#endif /* end of include guard: CSC_UTILS_MAP_H */
