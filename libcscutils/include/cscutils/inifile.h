/*
 * CSCUTILS - A collection of various software routines uses in CSC projects
 * Copyright (C) 2015 Martin Koehler
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

#ifndef CSC_INIFILE_H
#define CSC_INIFILE_H

#include "cscutils/cscutils_config.h"

#ifdef __cplusplus
extern "C" {
#endif
#include <pthread.h>

    /**
      @file libcscutils/include/cscutils/inifile.h
      @defgroup inifile IniFile: Reading and writing of key-value store configuration files.

      This part of the library contains routines to read and write
      init configuration files of like they are used on old Microsoft systems or inside the KDE configuration.

      Normally, such files look like:
      \verbatim
      [SECTION1]
      key1=value
      key2=value

      [SECTION2]
      key3=value
      \endverbatim
      where lines beginning with \b #, \b ;, or \b % are treated as comments.

      Additionally, so called \b nameless section are supported. That means that there are already values defined
      before the first section starts. In this case the file looks like:
      \verbatim
      keyA=value
      keyB=value

      [SECTION1]
      key1=value
      key2=value

      [SECTION2]
      key3=value
      \endverbatim
      This nameless section is access via \ref CSC_INI_DEFAULT_SECTION as section name.

      The \ref csc_ini_file_t represents such a file in memory. Futhermore,
      this group provides a set of function to handle such section-based
      key-value store.

      \attention
      Onlt the following functions are thread-safe:
      \li \ref csc_ini_getstring
      \li \ref csc_ini_getinteger
      \li \ref csc_ini_getfloat
      \li \ref csc_ini_setstring
      \li \ref csc_ini_setinteger
      \li \ref csc_ini_setfloat
      \li \ref csc_ini_section_add
      \li \ref csc_ini_section_remove
      \li \ref csc_ini_key_remove

      @addtogroup inifile
      @{
      */

    /**
     * @brief Alias for the nameless section
     *
     * Defines an alias for the \b nameless section. Use this as section name if you want to access the nameless section.
     */
#define CSC_INI_DEFAULT_SECTION ((char*) NULL)

    /**
     * @brief Iterator type to store the iteration state.
     *
     * This typedef defines the iterator state type. This is used by the functions \ref csc_ini_section_iterator and \ref csc_ini_kvstore_iterator to
     * store the current iteration state.
     */
    typedef void* csc_ini_iterator_t;


    /**
     * @brief Structure holding a key-value pair.
     *
     * The csc_ini_kvstore_t structure holds one key-value pair. It is used as one linked list per section.
     */
    typedef struct _csc_ini_kvstore_t {
        struct _csc_ini_kvstore_t * next;  /**< Pointer to the next element in the list */
        char * key;                        /**< String containing the key.  */
        char * value;                      /**< String containing the associated value. */
    } csc_ini_kvstore_t;


    struct _csc_ini_file_t;

    /**
     * @brief Structure representing a section in the configuration file.
     *
     * This structure represents a section of the configuration files. It is used as a linked list containing all
     * section. The key-value pairs of each section are stored as single linked list represented by \ref csc_ini_kvstore_t.
     */
    typedef struct _csc_ini_section_t {
        struct _csc_ini_section_t * next;   /**< Pointer to the next element in the list */
        char *section_name;                 /**< String containing the name of the section. In the case of the nameless section this is set to NULL. */
        csc_ini_kvstore_t * kvstore;        /**< Pointer to the head element of the key-value store \ref csc_ini_kvstore_t.  */
        struct _csc_ini_file_t *parent;      /**< Pointer to the parent configuration file structure. */
    } csc_ini_section_t;

    /**
     * @brief Structure representing a configuration file.
     *
     * The csc_ini_file_t structure represents a complete configuration file. It contains a single linked list with all sections. The structure
     * is either initialized using \ref csc_ini_empty or \ref csc_ini_load.
     *
     */
    typedef struct _csc_ini_file_t  {
        char * filename;                   /**< Name of the underlying configuration file, If the configuration is not created via \ref csc_ini_load then this is NULL. */
        csc_ini_section_t *sections;       /**< Pointer to the head of the linked list containing the sections. */
        csc_ini_section_t *section_tail;   /**< Pointer to the tail of the linked list containing the sections. */
        pthread_mutex_t lock;              /**< Mutex variable to lock the structure for some multi-thread accesses. */
        int changed;                       /**< True if a value had changed in the ini file. */
    } csc_ini_file_t ;

    /**
     * @brief Enumerator representing the return values.
     *
     * The csc_ini_error_t enumerator represents the different return values of the \b csc_ini functions.
     * */
    typedef enum {
        CSC_INI_SUCCESS = 0,    /**< The called function finished successful */
        CSC_INI_FILEOPEN,   /**< The called function failed to open the specified file. */
        CSC_INI_NOSECTION,      /**< The specified section was not found in the file. */
        CSC_INI_NOKEY,          /**< The specified key was not found in the selected section. */
        CSC_INI_VALUE,          /**< The value could not be converted to a number (integer or double )  */
        CSC_INI_MALLOC,         /**< A call to malloc failed. */
        CSC_INI_SYNTAX,         /**< A configuration file contains an invalid line. */
        CSC_INI_NULL            /**< An unexpected NULL-pointer occurred. */
    } csc_ini_error_t;

    /**
     * Flag to advise \ref csc_ini_load to convert all read section names to upper case.
     * */
#define CSC_INI_LOAD_SECTION_UPPERCASE  0x01
    /**
     * Flag to advise \ref csc_ini_load to convert all read section names to lower case.
     * */
#define CSC_INI_LOAD_SECTION_LOWERCASE  0x02
    /**
     * Flag to advise \ref csc_ini_load to convert all read key names to upper case.
     * */
#define CSC_INI_LOAD_KEY_UPPERCASE  0x04
    /**
     * Flag to advise \ref csc_ini_load to convert all read key names to lower case.
     * */
#define CSC_INI_LOAD_KEY_LOWERCASE  0x08


    /**
     * @brief Initialize an empty configuration file.
     * @param[out]  ini  Pointer to the \ref csc_ini_file_t structure.
     * @return A \ref csc_ini_error_t error value.
     *
     * The csc_ini_empty function initializes a \ref csc_ini_file_t structure such that it
     * can be used as empty configuration database. The function only initializes the basic
     * components of the structure. It does not free an existing one. For this case use
     * \ref csc_ini_free instead.
     *
     * \see csc_ini_load
     * \see csc_ini_free
     */
    csc_ini_error_t  csc_ini_empty(csc_ini_file_t *ini);

    /**
     * @brief Load a configuration file.
     * @param[in]   filename  Filename of the configuration file.
     * @param[in,out]  ini       Pointer to the \ref csc_ini_file_t structure.
     * @param[in,out]  flag      Flag to handle read values.
     * @return A \ref csc_ini_error_t error value.
     *
     * The csc_ini_load function loads a configuration file and store the information in a \ref csc_ini_file_t
     * structure. If the ini parameter refers to an already existing configuration file the content of the newly read file is added and
     * already existing keys are overwritten.  If a new file should be read, call \ref csc_ini_empty on this structure before.
     * The \c flag parameter can be used to modify the way how section names and keys are stored. The default, if flag==0, is that
     * they are stored as they are read. Specifying \ref CSC_INI_LOAD_SECTION_UPPERCASE or \ref CSC_INI_LOAD_KEY_UPPERCASE, the section
     * name or respectively the keys are stored in upper case. Using \ref CSC_INI_LOAD_SECTION_LOWERCASE or \ref CSC_INI_LOAD_KEY_LOWERCASE
     * this can be done on lower case. The flags can be or'ed together as long as they make sense.
     *
     * @see csc_ini_empty
     */
    csc_ini_error_t  csc_ini_load(const char *filename, csc_ini_file_t * ini, unsigned int flag);

    /**
     * @brief Saves a configuration file.
     * @param[in]  ini  Pointer to the \ref csc_ini_file_t structure.
     * @return A \ref csc_ini_error_t error value.
     *
     * The csc_ini_save function saves the given configuration to the file where it is load from. If the \ref csc_ini_load
     * function is called more the one time. The last file which was read is used. If the configuration is not created
     * by loading a file the function will return \ref CSC_INI_NULL. In this case one should use \ref csc_ini_write instead.
     *
     * @see csc_ini_write
     * @see csc_ini_load
     */
    csc_ini_error_t  csc_ini_save(csc_ini_file_t *ini);

    /**
     * @brief Write a configuration to a new file.
     * @param[in]   filename   Filename of the configuration file to be written.
     * @param[in]  ini  Pointer to the \ref csc_ini_file_t structure.
     * @return A \ref csc_ini_error_t error value.
     *
     * The csc_ini_write function write a configuration to a new configuration file. In contrast to the \ref csc_ini_save function
     * it does not use the filename provided in the \ref csc_ini_file_t structure. After writing the configuration data to the it
     * updates the filename stored in the \ref csc_ini_file_t structure.
     *
     * @see csc_ini_save
     * @see csc_ini_load
     */
    csc_ini_error_t  csc_ini_write(const char * filename, csc_ini_file_t *ini);

    /**
     * @brief Frees a configuration file.
     * @param[in]  ini  Pointer to the \ref csc_ini_file_t structure.
     * @return A \ref csc_ini_error_t error value.
     *
     * The csc_ini_free function removes a configuration from memory. If the \ref csc_ini_file_t is used afterwards again,
     * call \ref csc_ini_empty again on it.
     */
    csc_ini_error_t  csc_ini_free(csc_ini_file_t *ini);

    /**
     * @brief Check if a configuration file was changed.
     * @param[in]  ini  Pointer to the \ref csc_ini_file_t structure.
     * @return Zero if the file is unchanged, a positive value if the configuration file has been altered, and a negative value in case of an error.
     *
     * The csc_ini_has_changed function indicated if a value was written or has been altered in a configuration file.
     */
    int csc_ini_has_changed(csc_ini_file_t *ini);

    /**
     * @brief Get a string-valued entry from a configuration file.
     * @param[in]  ini     Pointer to the \ref csc_ini_file_t structure.
     * @param[in]  section Name of the section where to search for the key.
     * @param[in]  key     Key to search for.
     * @param[out] value   Pointer to the value of the configuration entry.
     * @return A \ref csc_ini_error_t error value.
     *
     * The csc_ini_getstring function search of a key in the given section and returns a pointer to the string-valued
     * content of the key. If the key or the section is not found the corresponding error (out of \ref csc_ini_error_t) is
     * returned. The returned string must be used read only. If you plan to modify an entry in the configuration file
     * use \ref csc_ini_setstring instead. If integer or floating point values should be read use \ref csc_ini_getinteger or
     * respectively csc_ini_getfloat instead.
     *
     * @remark The \b nameless section is accessed by using \ref CSC_INI_DEFAULT_SECTION as section.
     *
     * @see csc_ini_setstring
     * @see csc_ini_getinteger
     * @see csc_ini_getfloat
     */
    csc_ini_error_t  csc_ini_getstring(csc_ini_file_t * ini, const char *section, const char * key, char ** value);

    /**
     * @brief Get an integer-valued entry from a configuration file.
     * @param[in]  ini     Pointer to the \ref csc_ini_file_t structure.
     * @param[in]  section Name of the section where to search for the key.
     * @param[in]  key     Key to search for.
     * @param[out] value   Pointer to the value of the configuration entry.
     * @return A \ref csc_ini_error_t error value.
     *
     * The csc_ini_getinteger function search of a key in the given section and returns the integer-valued
     * content of the key. If the key or the section is not found the corresponding error (out of \ref csc_ini_error_t) is
     * returned. If it is not possible to convert the content of the entry to an integer \ref CSC_INI_VALUE is returned.
     * If strings or floating point values should be read use \ref csc_ini_getstring or respectively csc_ini_getfloat instead.
     *
     * @remark The \b nameless section is accessed by using \ref CSC_INI_DEFAULT_SECTION as section.
     *
     * @see csc_ini_setinteger
     * @see csc_ini_getstring
     * @see csc_ini_getfloat
     */
    csc_ini_error_t  csc_ini_getinteger(csc_ini_file_t * ini, const char *section, const char *key, int * value);

    /**
     * @brief Get a double-valued entry from a configuration file.
     * @param[in]  ini     Pointer to the \ref csc_ini_file_t structure.
     * @param[in]  section Name of the section where to search for the key.
     * @param[in]  key     Key to search for.
     * @param[out] value   Pointer to the value of the configuration entry.
     * @return A \ref csc_ini_error_t error value.
     *
     * The csc_ini_getfloat function search of a key in the given section and returns the double-valued
     * content of the key. If the key or the section is not found the corresponding error (out of \ref csc_ini_error_t) is
     * returned. If it is not possible to convert the content of the entry to a double precision number \ref CSC_INI_VALUE is returned.
     * If strings or integer values should be read use \ref csc_ini_getstring or respectively csc_ini_getinteger instead.
     *
     * @remark The \b nameless section is accessed by using \ref CSC_INI_DEFAULT_SECTION as section.
     *
     * @see csc_ini_setfloat
     * @see csc_ini_getstring
     * @see csc_ini_getinteger
     */
    csc_ini_error_t  csc_ini_getfloat(csc_ini_file_t *ini, const char *section, const char *key, double *value);


    /**
     * @brief Create or modify a string-valued key.
     * @param[in]  ini  Pointer to the \ref csc_ini_file_t structure.
     * @param[in]  section Name of the section where to search for the key.
     * @param[in]  key     Key to search for.
     * @param[in]  value   Pointer to the new value of the configuration entry.
     * @return A \ref csc_ini_error_t error value.
     *
     * The csc_ini_setstring function create a new or updates an existing string-valued entry in the given section of the configuration file.
     * If the key does not exist in the specified section a new one will be created. If the section does not exist the section
     * will be created as well. If the key already exists it is updated. Integer and floating point values can be set using \ref csc_ini_setinteger or
     * respectively \ref csc_ini_setfloat.
     *
     * @remark The \b nameless section is accessed by using \ref CSC_INI_DEFAULT_SECTION as section.
     *
     * @see csc_ini_getstring
     * @see csc_ini_setinteger
     * @see csc_ini_setfloat
     */
    csc_ini_error_t  csc_ini_setstring (csc_ini_file_t * ini, const char *section, const char *key, const char *value);

    /**
     * @brief Create or modify an integer-valued key.
     * @param[in]  ini  Pointer to the \ref csc_ini_file_t structure.
     * @param[in]  section Name of the section where to search for the key.
     * @param[in]  key     Key to search for.
     * @param[in]  value   Pointer to the new value of the configuration entry.
     * @return A \ref csc_ini_error_t error value.
     *
     * The csc_ini_setinteger function create a new or updates an existing integer-valued entry in the given section of the configuration file.
     * If the key does not exist in the specified section a new one will be created. If the section does not exist the section
     * will be created as well. If the key already exists it is updated. Strings and floating point values can be set using \ref csc_ini_setstring or
     * respectively \ref csc_ini_setfloat.
     *
     * @remark The \b nameless section is accessed by using \ref CSC_INI_DEFAULT_SECTION as section.
     *
     * @see csc_ini_getinteger
     * @see csc_ini_setstring
     * @see csc_ini_setfloat
     */
    csc_ini_error_t  csc_ini_setinteger(csc_ini_file_t * ini, const char *section, const char *key, const int value);

    /**
     * @brief Create or modify a double-valued key.
     * @param[in]  ini  Pointer to the \ref csc_ini_file_t structure.
     * @param[in]  section Name of the section where to search for the key.
     * @param[in]  key     Key to search for.
     * @param[in]  value   Pointer to the new value of the configuration entry.
     * @return A \ref csc_ini_error_t error value.
     *
     * The csc_ini_setfloat function create a new or updates an existing double-valued entry in the given section of the configuration file.
     * If the key does not exist in the specified section a new one will be created. If the section does not exist the section
     * will be created as well. If the key already exists it is updated. Strings and integer values can be set using \ref csc_ini_setstring or
     * respectively \ref csc_ini_setinteger.
     *
     * @remark The \b nameless section is accessed by using \ref CSC_INI_DEFAULT_SECTION as section.
     *
     * @see csc_ini_getfloat
     * @see csc_ini_setstring
     * @see csc_ini_setinteger
     */
    csc_ini_error_t  csc_ini_setfloat  (csc_ini_file_t * ini, const char *section, const char *key, const double value);

    /**
     * @brief Create an iterator over all sections of a configuration file.
     * @param[in]  ini  Pointer to the \ref csc_ini_file_t structure.
     * @param[in,out] iter Pointer to an iterator object.
     * @return The next section in the iterator or NULL if all sections are returned.
     *
     * The csc_ini_section_iterator function creates an iterator over all section in a configuration file. Therefore,
     * before the first call the iterator object must be set to zero and than reused for every call. On each subsequent
     * call the function will return the pointer to the next section in the configuration file. If all sections are returned
     * a NULL value will be returned to mark the end of iteration. The return value is pointer to a \ref csc_ini_section_t structure.
     *
     * This can be used to dump a whole configuration file:
     * \code{.c}
     * csc_ini_iterator_t iter = NULL;
     * csc_ini_iterator_t kv_iter = NULL;
     * csc_ini_section_t  *sec;
     * csc_ini_kvstore_t  *kv;
     *
     * while ((sec = csc_ini_section_iterator(ini, &iter)) != NULL ){
     *  printf("-> Section \"%s\"\n", csc_ini_getsectionname(sec));
     *  kv_iter = NULL;
     *  while ( (kv = csc_ini_kvstore_iterator(sec, &kv_iter)) != NULL)
     *      printf("--> %20s = %s\n", csc_ini_getkey(kv) , csc_ini_getvalue(kv));
     * }
     * \endcode
     *
     * @see csc_ini_kvstore_iterator
     * @see csc_ini_getsection
     */
    csc_ini_section_t * csc_ini_section_iterator(csc_ini_file_t *ini, csc_ini_iterator_t *iter);

    /**
     * @brief Search for a section inside a configuration file.
     * @param[in]  ini  Pointer to the \ref csc_ini_file_t structure.
     * @param[in]  section Name of the desired section.
     * @return Either the pointer to the wanted section or NULL if the section was not found.
     *
     * The csc_ini_getsection function search for a given section. If the section is found
     * in the configuration file a pointer to the section is returned. If the section is not found
     * or an error occurred NULL is returned.
     *
     * @remark The \b nameless section is accessed by using \ref CSC_INI_DEFAULT_SECTION as section.
     *
     * @see csc_ini_section_iterator
     */
    csc_ini_section_t * csc_ini_getsection      (csc_ini_file_t *ini, const char *section);

    /**
     * @brief Add a new empty section to a configuration file.
     * @param[in]  ini  Pointer to the \ref csc_ini_file_t structure.
     * @param[in]  section Name of the section to add.
     * @return A \ref csc_ini_error_t error value.
     *
     * The csc_ini_section_add function adds a new empty section to a configuration file. If the section already exists
     * nothing is done.
     *
     * @remark The \b nameless section is accessed by using \ref CSC_INI_DEFAULT_SECTION as section.
     *
     * @see csc_ini_section_remove
     */
    csc_ini_error_t csc_ini_section_add(csc_ini_file_t * ini, const char * section);

    /**
     * @brief Remove a section include the value from configuration file.
     * @param[in]  ini  Pointer to the \ref csc_ini_file_t structure.
     * @param[in]  section Name of the section to delete.
     * @return A \ref csc_ini_error_t error value.
     *
     * The csc_ini_section_remove function remove a section from a configuration file. All stored key-value pairs
     * will be removed as well.
     *
     * @remark The \b nameless section is accessed by using \ref CSC_INI_DEFAULT_SECTION as section.
     *
     * @see csc_ini_section_add
     */
    csc_ini_error_t csc_ini_section_remove(csc_ini_file_t * ini, const char * section);

    /**
     * @brief Get a string-valued entry from a section
     * @param[in]  section Pointer to the section structure.
     * @param[in]  key     Key to search for.
     * @param[out] value   Pointer to the value of the configuration entry.
     * @return A \ref csc_ini_error_t error value.
     *
     * The csc_ini_section_getstring  function search of a key in the given section and returns a pointer to the string-valued
     * content of the key. If the key or the section is not found the corresponding error (out of \ref csc_ini_error_t) is
     * returned. The returned string must be used read only. In contrast to the \ref csc_ini_getstring function this one
     * only works on one section instead of a whole configuration file.
     *
     * If you plan to modify an entry in the configuration file  use \ref csc_ini_section_setstring instead.
     * If integer or floating point values should be read use \ref csc_ini_section_getinteger or
     * respectively csc_ini_section_getfloat instead.
     *
     * @see csc_ini_section_setstring
     * @see csc_ini_section_getinteger
     * @see csc_ini_section_getfloat
     */
    csc_ini_error_t  csc_ini_section_getstring (csc_ini_section_t * section, const char * key, char ** value);

    /**
     * @brief Get a integer-valued entry from a section
     * @param[in]  section Pointer to the section structure.
     * @param[in]  key     Key to search for.
     * @param[out] value   Pointer to the value of the configuration entry.
     * @return A \ref csc_ini_error_t error value.
     *
     * The csc_ini_section_getinteger function search of a key in the given section and returns the integer-valued
     * content of the key. If the key or the section is not found the corresponding error (out of \ref csc_ini_error_t) is
     * returned. If the content could not be converted to an integer \ref CSC_INI_VALUE is returned.
     * In contrast to the \ref csc_ini_getinteger function this one only works on one section instead of a whole configuration file.
     *
     * If you plan to modify an entry in the configuration file  use \ref csc_ini_section_setinteger instead.
     * If strings or floating point values should be read use \ref csc_ini_section_getstring or
     * respectively csc_ini_section_getfloat instead.
     *
     * @see csc_ini_section_setinteger
     * @see csc_ini_section_getstring
     * @see csc_ini_section_getfloat
     */
    csc_ini_error_t  csc_ini_section_getinteger(csc_ini_section_t * section, const char * key, int * value);

    /**
     * @brief Get a double-valued entry from a section
     * @param[in]  section Pointer to the section structure.
     * @param[in]  key     Key to search for.
     * @param[out] value   Pointer to the value of the configuration entry.
     * @return A \ref csc_ini_error_t error value.
     *
     * The csc_ini_section_getfloat function search of a key in the given section and returns the double-valued
     * content of the key. If the key or the section is not found the corresponding error (out of \ref csc_ini_error_t) is
     * returned. If the content could not be converted to a double precision number \ref CSC_INI_VALUE is returned.
     * In contrast to the \ref csc_ini_getfloat function this one only works on one section instead of a whole configuration file.
     *
     * If you plan to modify an entry in the configuration file  use \ref csc_ini_section_setfloat instead.
     * If strings or integer  values should be read use \ref csc_ini_section_getstring or
     * respectively csc_ini_section_getint instead.
     *
     * @see csc_ini_section_setfloat
     * @see csc_ini_section_getstring
     * @see csc_ini_section_getinteger
     */
    csc_ini_error_t  csc_ini_section_getfloat  (csc_ini_section_t * section, const char * key, double *value);


    /**
     * @brief Create or modify a string-valued key in a section .
     * @param[in]  section Pointer to the section structure.
     * @param[in]  key     Key to search for.
     * @param[in] value   Pointer to the value of the configuration entry.
     * @return A \ref csc_ini_error_t error value.
     *
     * The csc_ini_section_setstring function create a new or updates an existing string-valued entry in the given section,
     * If the key does not exist in the specified section a new one will be created. In contrast to the \ref csc_ini_setstring
     * function this one works directly on the section and not on the whole configuration file.  If the key already exist the
     * value is updated.
     * Integer and floating point values can be set using \ref csc_ini_section_setinteger or
     * respectively \ref csc_ini_section_setfloat.
     *
     * @see csc_ini_section_getstring
     * @see csc_ini_section_setinteger
     * @see csc_ini_section_setfloat
     */
    csc_ini_error_t  csc_ini_section_setstring (csc_ini_section_t * section, const char * key, const char * value);

    /**
     * @brief Create or modify a integer-valued key in a section .
     * @param[in]  section Pointer to the section structure.
     * @param[in]  key     Key to search for.
     * @param[in] value   Pointer to the value of the configuration entry.
     * @return A \ref csc_ini_error_t error value.
     *
     * The csc_ini_section_setinteger function create a new or updates an existing integer-valued entry in the given section,
     * If the key does not exist in the specified section a new one will be created. In contrast to the \ref csc_ini_setinteger
     * function this one works directly on the section and not on the whole configuration file.  If the key already exist the
     * value is updated.
     * Strings and floating point values can be set using \ref csc_ini_section_setstring or
     * respectively \ref csc_ini_section_setfloat.
     *
     * @see csc_ini_section_getinteger
     * @see csc_ini_section_setstring
     * @see csc_ini_section_setfloat
     */
    csc_ini_error_t  csc_ini_section_setinteger(csc_ini_section_t * section, const char * key, const int value);

    /**
     * @brief Create or modify a double-valued key in a section .
     * @param[in]  section Pointer to the section structure.
     * @param[in]  key     Key to search for.
     * @param[in] value   Pointer to the value of the configuration entry.
     * @return A \ref csc_ini_error_t error value.
     *
     * The csc_ini_section_setfloat function create a new or updates an existing double-valued entry in the given section,
     * If the key does not exist in the specified section a new one will be created. In contrast to the \ref csc_ini_setfloat
     * function this one works directly on the section and not on the whole configuration file.  If the key already exist the
     * value is updated.
     * Strings and integer values can be set using \ref csc_ini_section_setstring or
     * respectively \ref csc_ini_section_setinteger.
     *
     * @see csc_ini_section_getfloat
     * @see csc_ini_section_setstring
     * @see csc_ini_section_setinteger
     */
    csc_ini_error_t  csc_ini_section_setfloat  (csc_ini_section_t * section, const char * key, const double value);

    /**
     * @brief Create an iterator over all keys of a section.
     * @param[in]  section  Pointer to the section.
     * @param[in,out] iter Pointer to an iterator object.
     * @return The next key-value pair in the iterator or NULL if all pairs are returned.
     *
     * The csc_ini_kvstore_iterator function creates an iterator over all key-value pairs in a given section. The section must
     * be given as a pointer to the corresponding \ref csc_ini_section_t structure.  On the first the iterator object must be set to zero
     * and than reused for every call. On each subsequent  all the function will return the pointer to the next key-value pair in the section.
     * If all key-value pairs are returned a NULL value will be returned to mark the end of iteration.
     * The return value is pointer to a \ref csc_ini_kvstore_t structure.
     *
     * An example is shown in \ref csc_ini_section_iterator.
     *
     * @see csc_ini_section_iterator
     * @see csc_ini_getsection
     */
    csc_ini_kvstore_t * csc_ini_kvstore_iterator(csc_ini_section_t *section, csc_ini_iterator_t *iter);

    /**
     * @brief Remove a key from a configuration file.
     * @param[in]  ini  Pointer to the \ref csc_ini_file_t structure.
     * @param[in]  section Name of the section to search for the key.
     * @param[in]  key     Name of the key to remove
     * @return A \ref csc_ini_error_t error value.
     *
     * The csc_ini_key_remove function removes a key from a configuration file. It search for the key in the
     * given section and removes it.
     *
     * @remark The \b nameless section is accessed by using \ref CSC_INI_DEFAULT_SECTION as section.
     *
     * @see csc_ini_section_key_remove
     * @see csc_ini_section_remove
     */
    csc_ini_error_t  csc_ini_key_remove(csc_ini_file_t *ini, const char * section, const char *key);

    /**
     * @brief Remove a key from a section
     * @param[in]  section  Pointer to the section.
     * @param[in]  key     Name of the key to remove
     * @return A \ref csc_ini_error_t error value.
     *
     * The csc_ini_key_remove function removes a key from a section. The section is given as a pointer to
     * a \ref csc_ini_section_t structure.
     *
     *
     * @see csc_ini_section_key_remove
     * @see csc_ini_section_remove
     * @see csc_ini_getsection
     */
    csc_ini_error_t  csc_ini_section_key_remove(csc_ini_section_t *section, const char *key);


    /**
     * @brief Return the key of a key-value pair.
     * @param[in] kvstore Pointer to the key-value pair.
     * @return A string containing the key of the key-value pair.
     *
     * The csc_ini_getkey returns the key of a key-value pair. This should be used instead of accessing
     * the \ref csc_ini_kvstore_t structure directly.
     *
     * @see csc_ini_getvalue
     */
    char * csc_ini_getkey(const csc_ini_kvstore_t * kvstore);

    /**
     * @brief Return the value of a key-value pair.
     * @param[in] kvstore Pointer to the key-value pair.
     * @return A string containing the value of the key-value pair.
     *
     * The csc_ini_getvalue returns the key of a key-value pair. This should be used instead of accessing
     * the \ref csc_ini_kvstore_t structure directly.
     *
     * @see csc_ini_getkey
     */
    char * csc_ini_getvalue(const csc_ini_kvstore_t * kvstore);

    /**
     * @brief Return the name of a section.
     * @param[in] sec Pointer to an instance of \ref csc_ini_section_t.
     * @return A string containing the name of the section referred by sec.
     *
     * The csc_ini_getsectionname returns the name of the given section. This should be used instead of accessing
     * the \ref csc_ini_section_t structure directly.
     *
     * @see csc_ini_getsection
     */
    char * csc_ini_getsectionname(const csc_ini_section_t *sec);



    int csc_ini_easy_get_int(const char *filename, const char * section, const char *key, int *val);
    int csc_ini_easy_get_double(const char *filename, const char * section, const char *key, double *val);
    int csc_ini_easy_get_string(const char *filename, const char * section, const char *key, char *val, int maxlen);


    /** @}  */

#ifdef __cplusplus
};
#endif
#endif

