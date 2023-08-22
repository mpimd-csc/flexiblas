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
 * Copyright (C) Martin Koehler, 2013-2023
 */





#ifndef FLEXIBLAS_MGMT_H

#define FLEXIBLAS_MGMT_H

#ifdef __cplusplus
extern "C" {
#endif

    #define FLEXIBLAS_MGMT_MAX_BUFFER_LEN 32*1024

    /**
     * @brief Enumeration to identify the different config location.
     *
     * The flexiblas_mgmt_location_t enumeration defines the different possible
     * locations for the FlexiBLAS configuration file.
     */
    typedef enum {
        FLEXIBLAS_GLOBAL = 0,       /**<  The system-wide configuration file. */
        FLEXIBLAS_USER = 1,         /**<  The user configuration file. */
        FLEXIBLAS_HOST = 2,         /**<  The host configuration file. */
        FLEXIBLAS_ENV  = 3,         /**<  The environment supplied configuration file. */
        FLEXIBLAS_GLOBAL_DIR = 4,   /**<  The configuration comes from the global configuration dir and thus is read only. */
        FLEXIBLAS_DEFAULT = 5       /**<  The compiled in default, only used with properties. */
    } flexiblas_mgmt_location_t;
    #define FLEXIBLAS_MGMT_LOCATION_COUNT 5

    /**
     * @brief Enumeration to define the setable FlexiBLAS properties.
     *
     * The flexiblas_mgmt_property_t enumeration identies the possible
     * properties that can be set in the FlexiBLAS configuration.
     * */
    typedef enum {
        FLEXIBLAS_PROP_VERBOSE = 0,     /**< Toogle the verbosity of FlexiBLAS. The value is a standart C \b int. */
        FLEXIBLAS_PROP_NOLAPACK =1,     /**< Disable loading of LAPACK functions from backend. */
    } flexiblas_mgmt_property_t;

    /**
     * @brief Structure containing all three FlexiBLAS configurations.
     *
     * The flexiblas_mgmt_t structure collects all three possible FlexiBLAS configuration
     * files. For flexiblity and internal changes these are only void-pointers.
     */
    typedef struct _flexiblas_config_t {
        void *system_config; /**<  System config, usually etc/flexiblasrc */
        void *user_config;   /**<  User config, usually ~/.flexiblasrc */
        void *host_config;   /**<  Host config, usually ~/.flexiblasrc.HOSTNAME */
        void *env_config;    /**<  Enviroment-supplied config. */
        void *system_dir_config; /**< Global config directory, read only. */
        char **blas_names;   /**<  Array of char containing all backend names. */
        size_t  nblas_names; /**<  Number of elements in the blas_names array. */
    } flexiblas_mgmt_t;


    /**
     * @brief Return the location of a FlexiBLAS configuration file.
     * @parami[in] loc  Wanted FlexiBLAS configuration file
     * @return A malloced pointer to a string containing the path.
     *
     * The flexiblas_mgmt_location function returns a malloced string
     * containing the path to the desired FlexiBLAS configuration file.
     *
     * @see flexiblas_mgmt_location_t
     */
    char *flexiblas_mgmt_location(flexiblas_mgmt_location_t loc);


    /**
     * @brief Returns a humanreadable string from the location enumeration.
     * @param[in] loc       Location of the configuration.
     * @return A pointer to a static string inside a function.
     *
     * The flexiblas_mgmt_location_to_string function converts a given location into a humanreadable string.
     */
    char *flexiblas_mgmt_location_to_string(flexiblas_mgmt_location_t loc);

    void flexiblas_mgmt_init(void);
    void flexiblas_mgmt_exit(void);

    /**
     * @brief Loads the FlexiBLAS configuration.
     * @return A pointer to the FlexiBLAS configuration or NULL in case of an error.
     *
     * The flexiblas_mgmt_load_config function loads the configuration from all three
     * possible locations (\ref flexiblas_mgmt_location_t).
     */
    flexiblas_mgmt_t * flexiblas_mgmt_load_config(void);


    /**
     * @brief Write the FlexiBLAS configuration to the configuration files
     * @param[in] config   The FlexiBLAS configuration to write.
     * @return zero on success or a non zero error code otherwise
     *
     * The flexiblas_mgmt_write_config function write the configuration to all
     * locations. The system config is only written if it is accessible. If the
     * following postive return values can be or'ed together:
     * \li \b 0x01 The system config could not be written.
     * \li \b 0x02 The user config could not be written.
     * \li \b 0x04 The host config could not be written.
     */
    int flexiblas_mgmt_write_config(flexiblas_mgmt_t *config);


    /**
     * @brief Alternative function to write the FlexiBLAS configuration.
     * @param[in] config        The FlexiBLAS configuration
     * @param[in] loc           Location of the configuration to write.
     * @return zero on success or a non zero error code otherwise
     *
     * The flexiblas_mgmt_write_config2 function writes only the part of the configuration specified
     * by loc to the corresponding configuration file. In this way the \ref flexiblas_mgmt_write_config function
     * decomposes into
     * \code
     * flexiblas_mgmt_write_config2(config, FLEXIBLAS_GLOBAL);
     * flexiblas_mgmt_write_config2(config, FLEXIBLAS_USER);
     * flexiblas_mgmt_write_config2(config, FLEXIBLAS_HOST);
     * \endcode
     */
    int flexiblas_mgmt_write_config2(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc);

    /**
     * @brief Free a FlexiBLAS configuration structure.
     * @param[in]   config  Free the memory of the FlexiBLAS configuration.
     *
     * The flexiblas_mgmt_free_config function frees the memory used
     * by the FlexiBLAS configuration.
     */
    void flexiblas_mgmt_free_config(flexiblas_mgmt_t *config);

    int flexiblas_mgmt_update_name_list(flexiblas_mgmt_t *config) ;

    /* Access BLAS libraries  */

    /**
     * @brief List the BLAS libraries contained in the configuration.
     * @param[in] config    FlexiBLAS configuration to use.
     * @param[in] loc       Location of the config, either system wide config, user config or host config.
     * @param[out] blas_name Name of the BLAS backend.
     * @param[out] library   Name and path of the library of the BLAS backend.
     * @param[out] comment   Stored comment about the BLAS backend.
     * @param[inout] help    Help pointer storing the iteration state .
     * @return A negative value in case of an error. Zero is no entry is left anylonger or a positive value
     *         if it an entry could be retrieved successfully.
     *
     * The flexiblas_mgmt_list_blas function iterates over the BLAS libraries defined
     * in a given location. On first input the help pointer should be set to NULL. The buffers
     * for the character string must be at least \ref FLEXIBLAS_MGMT_MAX_BUFFER_LEN elements long.
     *
     * \code
     * void *iter_helper = NULL;
     * while ( flexiblas_mgmt_list_blas(config, FLEXIBLAS_GLOBAL, blas_name, library_name, comment, &iter_helper) > 0)
     * {
     * 		printf(" %s\n", blas_name);
     * 		if (strlen(library_name) == 0) {
     * 			printf("   not usable, library not set.\n");
     * 		} else {
     * 			printf("   library = %s\n", library_name);
     * 		}
     * 		printf("   comment = %s\n", library_name);
     * }
     * \endcode
     */
    int flexiblas_mgmt_list_blas(flexiblas_mgmt_t * config, flexiblas_mgmt_location_t loc,
                                char *blas_name, char * library, char *comment, void **help);

    /**
     * @brief List the compiled-in default search paths for libraries.
     * @param[out]  path     Stored path
     * @param[in,out] help   Void pointer to store the internal iteration space.
     * @return A negative value in case of an error. Zero is no entry is left anylonger or a positive value
     *         if it an entry could be retrieved successfully.
     *
     * The flexiblas_mgmt_list_default_paths function iterates of the compiled in default search
     * paths for shared libraries. The function works similar to the \ref flexiblas_mgmt_list_blas function.
     * The path buffer must be at least \ref FLEXIBLAS_MGMT_MAX_BUFFER_LEN elements long.
     */
    int flexiblas_mgmt_list_default_paths(char *path, void **help);

    /**
     * @brief List the additional shared library search paths stored in the configuration.
     * @param[in] config    FlexiBLAS configuration to use.
     * @param[in] loc       Location of the config, either system wide config, user config or host config.
     * @param[out]  path     Stored path
     * @param[in,out] help   Void pointer to store the internal iteration space.
     * @return A negative value in case of an error. Zero is no entry is left anylonger or a positive value
     *         if it an entry could be retrieved successfully.
     *
     * The flexiblas_mgmt_list_paths function iterates over the additional search paths stored in the configuration.
     * It works like \ref flexiblas_mgmt_list_default_paths or \ref flexiblas_mgmt_list_blas functions.
     * The path buffer must be at least \ref FLEXIBLAS_MGMT_MAX_BUFFER_LEN elements long.
     */
    int flexiblas_mgmt_list_paths(flexiblas_mgmt_t * config, flexiblas_mgmt_location_t loc,  char *path, void **help);


    /**
     * @brief Get the default BLAS backend stored in the configuration.
     * @param[in]   config      The FlexiBLAS configuration.
     * @param[in]   loc         Location of the config, either system wide config, user config or host config.
     * @param[out]  def         Name of the default BLAS backend in the configuration.
     * @return zero on success, a negative value on error and a postive value if no default is found
     *         in the configuration.
     *
     * The flexiblas_mgmt_get_default function returns the default BLAS backend given in a configuration.
     * The buffer def must be at least \ref FLEXIBLAS_MGMT_MAX_BUFFER_LEN elements long.
     */
    int flexiblas_mgmt_get_default(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc, char *def );


    /**
     * @brief Get the active default BLAS from the configuration.
     * @param[in]   config      The FlexiBLAS configuration.
     * @param[out]  loc         The location where the default is set.
     * @param[out]  def         The default BLAS backend selected by the configuration.
     * @return zero on success or a non zero error code otherwise
     *
     * The flexiblas_mgmt_get_active_default function gets the active default BLAS backend set in the configuration.
     * The buffer def must be at least \ref FLEXIBLAS_MGMT_MAX_BUFFER_LEN elements long.
     */
    int flexiblas_mgmt_get_active_default(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t *loc, char *def );


    /**
     * @brief Set the default BLAS in the configuration.
     * @param[in]   config      The FlexiBLAS configuration.
     * @param[in]  loc         The location where the default is set.
     * @param[in]  def         The default BLAS backend selected by the configuration.
     * @return zero on success or a non zero error code otherwise
     *
     * The flexiblas_mgmt_set_default function sets the default BLAS backend in the configuration.
     */
    int flexiblas_mgmt_set_default(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc,  char *def );


    /**
     * @brief Check if a BLAS exists in the configuration and return its location.
     * @param[in]   config      The FlexiBLAS configuration.
     * @param[in]   blas_name   The name of the backend to search for.
     * @param[out]  loc         Location where the BLAS backend is configured.
     * @return zero on error or if the BLAS backend was not found, a positive value otherwise.
     *
     * The flexiblas_mgmt_blas_exists function checks if a BLAS backend with a given name is found
     * somewhere in the configuration. If loc is not equal to NULL the location is returned by this parameter.
     */
    int flexiblas_mgmt_blas_exists(flexiblas_mgmt_t *config, char* blas_name, flexiblas_mgmt_location_t *loc);


    /**
     * @brief Add a new BLAS backend to the configuration.
     * @param[in]   config      The FlexiBLAS configuration.
     * @param[in]   loc         Location where the BLAS backend should be added to.
     * @param[in]   blas_name   Name of the new backend.
     * @param[in]   so_name     Name of the shared object.
     * @param[in]   comment     Comment for the backend.
     * @return zero on success or a non zero error code otherwise
     *
     * The flexiblas_mgmt_blas_add function adds a new BLAS backend to the configuration. The backend consists
     * of a shared object, a name and a comment and will be added as a new section to the configuration file.
     * If the backend already exists it is updated. The name is not case sensitive.
     */
    int flexiblas_mgmt_blas_add (flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc,
            char *name, char *so_name, char*comment);


    /**
     * @brief Remove a BLAS backend from the configuration.
     * @param[in] config        The FlexiBLAS configuration.
     * @param[in] loc           The location where to remove the backend from.
     * @param[in] name          Name of the backend to remove.
     * @return zero on success or a non zero error code otherwise
     *
     * The flexiblas_mgmt_blas_remove function remove a given BLAS backend from the configuration.
     */
    int flexiblas_mgmt_blas_remove(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc, char *name);

    /*  Access properties */

    /**
     * @brief Get the value of a property from the configuration.
     * @param[in]   config      The FlexiBLAS configuration.
     * @param[in]   loc         The location of the property in the configuration.
     * @param[in]   prop        The property to get.
     * @param[out]  buffer      Output buffer to write the configuration value to.
     * @return zero on success or a non zero error code otherwise
     *
     * The flexiblas_mgmt_get_property function gets the value of a property from the configuration. The type
     * of the buffer depends on the property and is given in the documentation of \ref flexiblas_mgmt_property_t .
     * In case of character string the buffer must be \ref FLEXIBLAS_MGMT_MAX_BUFFER_LEN elements long.
     */
    int flexiblas_mgmt_get_property(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc,
            flexiblas_mgmt_property_t prop, void *buffer);


    int flexiblas_mgmt_blas_get(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc,
            const char *blas_name, char *library, char *comment);
    int flexiblas_mgmt_blas_get2(flexiblas_mgmt_t *config,  flexiblas_mgmt_location_t *loc2,
            const char *blas_name, char *library, char *comment);

    /**
     * @brief Get the acvtive value of a property from the configuration.
     * @param[in]   config      The FlexiBLAS configuration.
     * @param[out]   loc         The location of the property in the configuration which is active.
     * @param[in]   prop        The property to get.
     * @param[out]  buffer      Output buffer to write the configuration value to.
     * @return zero on success or a non zero error code otherwise
     *
     * The flexiblas_mgmt_get_active_property function gets the active value of a property from the configuration.
     * The type of the buffer depends on the property and is given in the documentation of
     * \ref flexiblas_mgmt_property_t . In case of character string the buffer must be
     * \ref FLEXIBLAS_MGMT_MAX_BUFFER_LEN elements long.
     */
    int flexiblas_mgmt_get_active_property(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t *loc,
            flexiblas_mgmt_property_t prop, void *buffer);

    /**
     * @brief Set the value of a property from the configuration.
     * @param[in]   config      The FlexiBLAS configuration.
     * @param[in]   loc         The location of the property in the configuration.
     * @param[in]   prop        The property to get.
     * @param[in]   buffer      Input buffer to read the configuration value from.
     * @return zero on success or a non zero error code otherwise
     *
     * The flexiblas_mgmt_set_property function sets the value of a property in the configuration. The type
     * of the buffer depends on the property and is given in the documentation of \ref flexiblas_mgmt_property_t .
     * If the buffer is NULL it is reset to its compiled-in default.
     */
    int flexiblas_mgmt_set_property(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc,
            flexiblas_mgmt_property_t prop, void *buffer);


    /**
     * @brief Return the compiled-in default value of a property.
     * @param[in]   prop        The property to get.
     * @param[out]  buffer      Output buffer to write the default configuration value to.
     * @return zero on success or a non zero error code otherwise
     *
     * The flexiblas_mgmt_default_property function returns the compiled-in default for configuration values.
     * The type of the buffer depends on the property and is given in the documentation of
     * \ref flexiblas_mgmt_property_t . In case of character string the buffer must be
     * \ref FLEXIBLAS_MGMT_MAX_BUFFER_LEN elements long.
     */
    int flexiblas_mgmt_default_property(flexiblas_mgmt_property_t prop, void *buffer);

    /**
     * @brief Get an arbitrary key from a flexiblas config file.
     * @param[in]   config   FlexiBLAS configuration object.
     * @param[in]   loc      Location of the configuration
     * @param[in]   section  Section in the key-value storage file
     * @param[in]   key      Key to get
     * @param[out]  buffer   Character buffer of length \ref FLEXIBLAS_MGMT_MAX_BUFFER_LEN
     * @return zero on success or a negative value on failure.
     *
     * The flexiblas_mgmt_get_key function returns an entry from the specified location and section
     * and copies the result to the specified buffer.
     **/
    int flexiblas_mgmt_get_key(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc,
        char * section, char *key, char *buffer);


     /**
     * @brief Get an the active key from a flexiblas config file.
     * @param[in]   config   FlexiBLAS configuration object.
     * @param[out]   loc      Location of the key which is active.
     * @param[in]   section  Section in the key-value storage file
     * @param[in]   key      Key to get
     * @param[out]  buffer   Character buffer of length \ref FLEXIBLAS_MGMT_MAX_BUFFER_LEN
     * @return zero on success or a negative value on failure.
     *
     * The flexiblas_mgmt_get_active_key returns an entry from the specified section
     * and copies the result to the specified buffer. Furthermore, it returns the location where
     * the key is set.
     **/
    int flexiblas_mgmt_get_active_key(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t *loc,
            char *section, char *key, char*buffer);


    /**
     * @brief Get a hook option from a given config and location. (Integer return value)
     * @param[in] config    The FlexiBLAS configuration
     * @param[in] loc       Location where to search for the hook.
     * @param[in] hook      Name of the hook in the configuration.
     * @param[in] option    Option to get.
     * @param[out] val      Integer value from the config.
     * @return 0 on success, non zero otherwise.
     *
     * The flexiblas_mgmt_hook_option_get_int_loc searches for a hook option inside the
     * configuration with a given location and returns its value.
     *
     * \sa flexiblas_mgmt_hook_option_get_string_loc
     * \sa flexiblas_mgmt_hook_option_get_float_loc
     * \sa flexiblas_mgmt_hook_option_get_int
     * \sa flexiblas_mgmt_hook_option_get_string
     * \sa flexiblas_mgmt_hook_option_get_float
     */
    int flexiblas_mgmt_hook_option_get_int_loc(flexiblas_mgmt_t * config, flexiblas_mgmt_location_t loc, char * hook, char *option, int *val);

    /**
     * @brief Get a hook option from a given config and location. (String return value)
     * @param[in] config    The FlexiBLAS configuration
     * @param[in] loc       Location where to search for the hook.
     * @param[in] hook      Name of the hook in the configuration.
     * @param[in] option    Option to get.
     * @param[out] str      String found in the configuration. Needs to be preallocated of dimension at least FLEXIBLAS_MGMT_MAX_BUFFER_LEN
     * @return 0 on success, non zero otherwise.
     *
     * The flexiblas_mgmt_hook_option_get_string_loc searches for a hook option inside the
     * configuration with a given location and returns its value.
     *
     * \sa flexiblas_mgmt_hook_option_get_int_loc
     * \sa flexiblas_mgmt_hook_option_get_float_loc
     * \sa flexiblas_mgmt_hook_option_get_int
     * \sa flexiblas_mgmt_hook_option_get_string
     * \sa flexiblas_mgmt_hook_option_get_float
     *
     */
    int flexiblas_mgmt_hook_option_get_string_loc(flexiblas_mgmt_t * config, flexiblas_mgmt_location_t loc, char * hook, char *option, char *str);

    /**
     * @brief Get a hook option from a given config and location. (Double Precision return value)
     * @param[in] config    The FlexiBLAS configuration
     * @param[in] loc       Location where to search for the hook.
     * @param[in] hook      Name of the hook in the configuration.
     * @param[in] option    Option to get.
     * @param[out] val      Double precision value found in the configuration.
     * @return 0 on success, non zero otherwise.
     *
     * The flexiblas_mgmt_hook_option_get_float_loc searches for a hook option inside the
     * configuration with a given location and returns its value.
     *
     * \sa flexiblas_mgmt_hook_option_get_int_loc
     * \sa flexiblas_mgmt_hook_option_get_string_loc
     * \sa flexiblas_mgmt_hook_option_get_int
     * \sa flexiblas_mgmt_hook_option_get_string
     * \sa flexiblas_mgmt_hook_option_get_float
     *
     */
    int flexiblas_mgmt_hook_option_get_float_loc(flexiblas_mgmt_t * config, flexiblas_mgmt_location_t loc, char * hook, char *option, double *val);

    /**
     * @brief Get the active hook option from a given config (Integer return value)
     * @param[in] config    The FlexiBLAS configuration
     * @param[in] hook      Name of the hook in the configuration.
     * @param[in] option    Option to get.
     * @param[out] val      Integer return value.
     * @return 0 on success, non zero otherwise.
     *
     * The flexiblas_mgmt_hook_option_get_int function gets the active value of a option of a hook
     * from the configuration. Active means that the value of the option is used being set in the
     * last loaded configuration. The load order is System-Wide, User config, Host Config.
     *
     * \sa flexiblas_mgmt_hook_option_get_int_loc
     * \sa flexiblas_mgmt_hook_option_get_string_loc
     * \sa flexiblas_mgmt_hook_option_get_float_loc
     * \sa flexiblas_mgmt_hook_option_get_string
     * \sa flexiblas_mgmt_hook_option_get_float
     */
    int flexiblas_mgmt_hook_option_get_int(flexiblas_mgmt_t * config, char * hook, char *option, int *val);


    /**
     * @brief Get the active hook option from a given config (String return value)
     * @param[in] config    The FlexiBLAS configuration
     * @param[in] hook      Name of the hook in the configuration.
     * @param[in] option    Option to get.
     * @param[out] str      String return value.
     * @return 0 on success, non zero otherwise.
     *
     * The flexiblas_mgmt_hook_option_get_string function gets the active value of a option of a hook
     * from the configuration. Active means that the value of the option is used being set in the
     * last loaded configuration. The load order is System-Wide, User config, Host Config.
     *
     * \sa flexiblas_mgmt_hook_option_get_int_loc
     * \sa flexiblas_mgmt_hook_option_get_string_loc
     * \sa flexiblas_mgmt_hook_option_get_float_loc
     * \sa flexiblas_mgmt_hook_option_get_int
     * \sa flexiblas_mgmt_hook_option_get_float
     */
    int flexiblas_mgmt_hook_option_get_string(flexiblas_mgmt_t * config, char * hook, char *option, char *str);


    /**
     * @brief Get the active hook option from a given config (Double precision return value)
     * @param[in] config    The FlexiBLAS configuration
     * @param[in] hook      Name of the hook in the configuration.
     * @param[in] option    Option to get.
     * @param[out] val      String return value.
     * @return 0 on success, non zero otherwise.
     *
     * The flexiblas_mgmt_hook_option_get_float function gets the active value of a option of a hook
     * from the configuration. Active means that the value of the option is used being set in the
     * last loaded configuration. The load order is System-Wide, User config, Host Config.
     *
     * \sa flexiblas_mgmt_hook_option_get_int_loc
     * \sa flexiblas_mgmt_hook_option_get_string_loc
     * \sa flexiblas_mgmt_hook_option_get_float_loc
     * \sa flexiblas_mgmt_hook_option_get_int
     * \sa flexiblas_mgmt_hook_option_get_float
     */
    int flexiblas_mgmt_hook_option_get_float(flexiblas_mgmt_t * config, char * hook, char *option, double *val);

    /**
     * @brief Set a option for a hook in the config.
     * @param[in] config    The FlexiBLAS configuration
     * @param[in] loc       Location where to search for the hook.
     * @param[in] cfg_name  Name of the hook in the configuration.
     * @param[in] optname   Name of the option.
     * @param[in] optval    Value of the options.
     * @return 0 on success, non zero otherwise.
     *
     * The flexiblas_mgmt_hook_option_set_int function set an integer option value for a hook
     * inside the configuration.
     *
     * \sa flexiblas_mgmt_hook_option_set_string
     * \sa flexiblas_mgmt_hook_option_set_float
     */
    int flexiblas_mgmt_hook_option_set_int(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc,
        char *cfg_name, char *optname, int optval);

    /**
     * @brief Set a option for a hook in the config.
     * @param[in] config    The FlexiBLAS configuration
     * @param[in] loc       Location where to search for the hook.
     * @param[in] cfg_name  Name of the hook in the configuration.
     * @param[in] optname   Name of the option.
     * @param[in] optval    Value of the options.
     * @return 0 on success, non zero otherwise.
     *
     * The flexiblas_mgmt_hook_option_set_int function set a string option value for a hook
     * inside the configuration.
     *
     * \sa flexiblas_mgmt_hook_option_set_int
     * \sa flexiblas_mgmt_hook_option_set_float
     */
    int flexiblas_mgmt_hook_option_set_string(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc,
        char *cfg_name, char *optname, char* optval);

    /**
     * @brief Set a option for a hook in the config.
     * @param[in] config    The FlexiBLAS configuration
     * @param[in] loc       Location where to search for the hook.
     * @param[in] cfg_name  Name of the hook in the configuration.
     * @param[in] optname   Name of the option.
     * @param[in] optval    Value of the options.
     * @return 0 on success, non zero otherwise.
     *
     * The flexiblas_mgmt_hook_option_set_float function set an double precision option value for a hook
     * inside the configuration.
     *
     * \sa flexiblas_mgmt_hook_option_set_string
     * \sa flexiblas_mgmt_hook_option_set_int
     */

    int flexiblas_mgmt_hook_option_set_float(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc,
        char *cfg_name, char *optname, double optval);

    /**
     * @brief Remove an option setting for a hook in the config.
     * @param[in] config    The FlexiBLAS configuration
     * @param[in] loc       Location where to search for the hook.
     * @param[in] cfg_name  Name of the hook in the configuration.
     * @param[in] optname   Name of the option.
     * @return 0 on success, non zero otherwise.
     *
     * The flexiblas_mgmt_hook_option_unset function removes an option setting for a given hook from the config.
     *
     * \sa flexiblas_mgmt_hook_option_set_string
     * \sa flexiblas_mgmt_hook_option_set_int
     * \sa flexiblas_mgmt_hook_option_set_float
     */
    int flexiblas_mgmt_hook_option_unset(flexiblas_mgmt_t * config, flexiblas_mgmt_location_t loc, char *cfg_name, char *optname);

    int flexiblas_mgmt_hook_get_active(flexiblas_mgmt_t * config, flexiblas_mgmt_location_t *loc, int *nelem, char ***list );

    int flexiblas_mgmt_hook_get_active_internal(flexiblas_mgmt_t * config, flexiblas_mgmt_location_t loc, int *nelem, char ***list );

    int flexiblas_mgmt_hook_exists(flexiblas_mgmt_t *config, const char *name);

    int flexiblas_mgmt_hook_enable(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc, char *hook);

    int flexiblas_mgmt_hook_disable(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc, const char *hook);

    int flexiblas_mgmt_hook_disable_all(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc);

#ifdef __cplusplus
};
#endif

#endif /* end of include guard: FLEXIBLAS_MGMT_H */
