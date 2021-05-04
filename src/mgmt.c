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
#include <math.h>
#include <string.h>
#include <strings.h>

#include "flexiblas.h"
#include "flexiblas_mgmt.h"
#include "helper.h"
#include "hooks.h"
#include "paths.h"
#include "cscutils/strutils.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <dlfcn.h>


#define MAX_BUFFER_SIZE 32*1024

HIDDEN int __flexiblas_mgmt_init = 0;


void flexiblas_mgmt_init()
{

    flexiblas_mgmt_t * config = NULL;

    if ( __flexiblas_mgmt_init ) return;
    __flexiblas_mgmt_init = 1;

    config = flexiblas_mgmt_load_config();
    if ( config == NULL) return;


    /* Add additional search paths */
    __flexiblas_add_path_from_environment();
    __flexiblas_add_path_from_config(config, FLEXIBLAS_ENV);
    __flexiblas_add_path_from_config(config, FLEXIBLAS_HOST);
    __flexiblas_add_path_from_config(config, FLEXIBLAS_USER);
    __flexiblas_add_path_from_config(config, FLEXIBLAS_GLOBAL_DIR);
    __flexiblas_add_path_from_config(config, FLEXIBLAS_GLOBAL);
    __flexiblas_init_default_paths();


    /* Search all available hooks */
    __flexiblas_add_hooks();

    flexiblas_mgmt_free_config(config);

    return;
}


void flexiblas_mgmt_exit()
{
    if (! __flexiblas_mgmt_init ) return;
    __flexiblas_mgmt_init = 0;
    __flexiblas_free_paths();
    __flexiblas_exit_hook();
    return;
}

/*  Internal wrapper around getenv  */
static char *__flexiblas_mgmt_getenv(int what) {
	char container[MAX_BUFFER_SIZE];
	container[0] = '\0';
	switch (what) {
		case FLEXIBLAS_ENV_SO_EXTENSION:
			#ifdef __APPLE__
			snprintf(container, MAX_BUFFER_SIZE, ".dylib");
			#else
			#ifdef __WIN32__
			snprintf(container, MAX_BUFFER_SIZE, ".dll");
			#else
			snprintf(container, MAX_BUFFER_SIZE, ".so");
			#endif
			#endif
			break;
		case FLEXIBLAS_ENV_HOMEDIR:
			#ifdef __WIN32__
			snprintf(container,MAX_BUFFER_SIZE,"%s\\%s\\",getenv("HOMEDRIVE"),getenv("HOMEPATH"));
			#else
			snprintf(container,MAX_BUFFER_SIZE,"%s",getenv("HOME"));
			#endif
			break;
		case FLEXIBLAS_ENV_GLOBAL_RC:
			#ifdef __WIN32__
			snprintf(container,MAX_BUFFER_SIZE,"%s\\%s", getenv("SYSTEMROOT"), FLEXIBLAS_RC);
			#else
			snprintf(container,MAX_BUFFER_SIZE,"%s/%s",CMAKE_INSTALL_FULL_SYSCONFDIR,FLEXIBLAS_RC);
			#endif
			break;
        case FLEXIBLAS_ENV_GLOBAL_RC_DIR:
            #ifdef __WIN32__
            #warning NOT IMPLEMENTED
            #else
			snprintf(container,MAX_BUFFER_SIZE,"%s/%s/",CMAKE_INSTALL_FULL_SYSCONFDIR,FLEXIBLAS_RC_DIR);
            #endif
            break;
		case FLEXIBLAS_ENV_USER_RC:
			#ifdef __WIN32__
			snprintf(container,MAX_BUFFER_SIZE,"%s\\%s",getenv("APPDATA"), FLEXIBLAS_RC);
			#else
			snprintf(container,MAX_BUFFER_SIZE,"%s/.%s", getenv("HOME"), FLEXIBLAS_RC);
			#endif
			break;
        case FLEXIBLAS_ENV_HOST_RC:
    		#ifdef __WIN32__
            #error Not implemented
			#else
            {
                char hostname[MAX_BUFFER_SIZE-32];
                gethostname(hostname, MAX_BUFFER_SIZE-32);
    			snprintf(container,MAX_BUFFER_SIZE,"%s/.%s.%s", getenv("HOME"), FLEXIBLAS_RC, hostname);
            }
			#endif
			break;
        case FLEXIBLAS_ENV_ENV_RC:
            #ifndef __WIN32__
            {
                if ( getenv("FLEXIBLAS_CONFIG") != NULL ) {
                    snprintf(container, MAX_BUFFER_SIZE, "%s", getenv("FLEXIBLAS_CONFIG"));
                    csc_str_remove_char(container, '"');
                    csc_str_remove_char(container, '\"');
                } else {
                    return NULL;
                }
            }
            #endif
            break;
		default:
			return NULL;
	}
	return strdup(container);
}
#undef MAX_BUFFER_SIZE

/* Location to ini   */
static csc_ini_file_t * loc_to_ini (flexiblas_mgmt_location_t loc, flexiblas_mgmt_t *config) {
    csc_ini_file_t *ini;
    if ( loc == FLEXIBLAS_GLOBAL) {
        ini = ( csc_ini_file_t *) config->system_config;
    } else if ( loc == FLEXIBLAS_USER ) {
        ini = ( csc_ini_file_t *) config->user_config;
    } else if ( loc == FLEXIBLAS_HOST ) {
        ini = ( csc_ini_file_t *) config->host_config;
    } else if ( loc == FLEXIBLAS_ENV ) {
        ini = ( csc_ini_file_t *) config->env_config;
    } else if ( loc == FLEXIBLAS_GLOBAL_DIR) {
        ini = (csc_ini_file_t *) config->system_dir_config;
    } else {
        return NULL;
    }
    return ini;
}

static csc_ini_file_t * idx_to_ini(int i, flexiblas_mgmt_t * config)
{
    csc_ini_file_t *ini;
    if ( i == 0 ){
        ini = (csc_ini_file_t *) config->system_config;
    } else if (i == 1) {
        ini = (csc_ini_file_t *) config->system_dir_config;
    } else if (i == 2) {
        ini = (csc_ini_file_t *) config->user_config;
    } else if (i == 3) {
        ini = (csc_ini_file_t *) config->host_config;
    } else if (i == 4) {
        ini = (csc_ini_file_t *) config->env_config;
    } else {
        ini = NULL;
    }
    return ini;
}

/*  Return the location of the config file. */
char *flexiblas_mgmt_location(flexiblas_mgmt_location_t loc)
{
    if ( loc == FLEXIBLAS_GLOBAL ){
        return  __flexiblas_mgmt_getenv(FLEXIBLAS_ENV_GLOBAL_RC);
    } else if ( loc == FLEXIBLAS_GLOBAL_DIR) {
        return __flexiblas_mgmt_getenv(FLEXIBLAS_ENV_GLOBAL_RC_DIR);
    } else if ( loc == FLEXIBLAS_USER ) {
        return __flexiblas_mgmt_getenv(FLEXIBLAS_ENV_USER_RC);
    } else if (loc == FLEXIBLAS_HOST ) {
    	return __flexiblas_mgmt_getenv(FLEXIBLAS_ENV_HOST_RC);
    } else if (loc == FLEXIBLAS_ENV) {
        return __flexiblas_mgmt_getenv(FLEXIBLAS_ENV_ENV_RC);
    }
    return NULL;
}

/*  Create a human reable string from the config location enum. */
char *flexiblas_mgmt_location_to_string(flexiblas_mgmt_location_t loc){
    static char *system_str = "System";
    static char *user_str   = "User";
    static char *host_str   = "Host";
    static char *env_str    = "Enviroment";
    static char *system_dir_str = "System Directory";
    static char *default_str = "Compiled-in default";
    static char *lz = "";
    if ( loc == FLEXIBLAS_GLOBAL )
        return system_str;
    if ( loc == FLEXIBLAS_USER )
        return user_str;
    if ( loc == FLEXIBLAS_HOST)
        return host_str;
    if ( loc == FLEXIBLAS_ENV)
        return env_str;
    if ( loc == FLEXIBLAS_DEFAULT)
        return default_str;
    if ( loc == FLEXIBLAS_GLOBAL_DIR)
        return system_dir_str;
    return lz;
}

/*  Load all configfiles */
flexiblas_mgmt_t * flexiblas_mgmt_load_config()
{
    flexiblas_mgmt_t * config;
    char *path;
    csc_ini_file_t *ini;

    config = (flexiblas_mgmt_t *) malloc(sizeof(flexiblas_mgmt_t) * (1));
    if ( config == NULL ){
        return NULL;
    }
    memset(config, 0, sizeof(flexiblas_mgmt_t));
    config->nblas_names = 0;
    config->blas_names = NULL;
    /*-----------------------------------------------------------------------------
     *  Allocate memory
     *-----------------------------------------------------------------------------*/
    config->system_config = (csc_ini_file_t *) malloc(sizeof(csc_ini_file_t) * (1));
    config->system_dir_config = (csc_ini_file_t *) malloc(sizeof(csc_ini_file_t) * (1));
    config->user_config = (csc_ini_file_t *) malloc(sizeof(csc_ini_file_t) * (1));
    config->host_config = (csc_ini_file_t *) malloc(sizeof(csc_ini_file_t) * (1));
    config->env_config = (csc_ini_file_t *) malloc(sizeof(csc_ini_file_t) * (1));

    if ( config->system_config == NULL
            || config->system_dir_config == NULL
            || config->user_config == NULL
            || config->host_config == NULL
            || config->env_config  == NULL)
    {
        flexiblas_mgmt_free_config(config);
        return NULL;
    }

    /*-----------------------------------------------------------------------------
     *  Load
     *
     *  If FLEXIBLAS_CONFIG is set in the environment it is loaded last an overwrites
     *  everything.
     *-----------------------------------------------------------------------------*/
    /*  System file  */
    path = __flexiblas_mgmt_getenv(FLEXIBLAS_ENV_GLOBAL_RC);
    ini = (csc_ini_file_t *) config->system_config;
    csc_ini_empty(ini);
    if ( __flexiblas_file_exist(path)) {
        DPRINTF(1, "Load system config %s\n", path);
        csc_ini_load(path, ini, CSC_INI_LOAD_SECTION_UPPERCASE);
    } else {
        DPRINTF_WARN(1, "Config %s does not exist.\n", path);
    }
    if ( path ) free(path);

    /* System Config Dir  */
    path = __flexiblas_mgmt_getenv(FLEXIBLAS_ENV_GLOBAL_RC_DIR);
    ini = (csc_ini_file_t *) config->system_dir_config;
    csc_ini_empty(ini);
    if (__flexiblas_directory_exists(path))
    {
        DIR * dir = opendir(path);
        struct dirent *dentry;
        char *xpath;
        size_t xpath_len;

        while ((dentry = readdir(dir)) != NULL) {
            if ( ! __flexiblas_str_endwith(dentry->d_name, ".conf")) continue;
            xpath_len = strlen(path) + strlen(dentry->d_name) + 10;
            xpath = (char *) malloc(sizeof(char) * (xpath_len));
            if ( !xpath_len) {
                DPRINTF_ERROR(0,"Failed to allocate memory for path %s/%s. Skip file.\n", path, dentry->d_name);
                continue;
            }
            snprintf(xpath, xpath_len-1, "%s/%s", path, dentry->d_name);
            DPRINTF(1, "Load config: %s\n", xpath);
            csc_ini_load(xpath, ini, CSC_INI_LOAD_SECTION_UPPERCASE);
            free(xpath);
        }

       closedir(dir);
    } else {
        DPRINTF_WARN(1, "Configuration directory %s does not exists.\n",path);
    }
    if ( path ) free(path);


    /* User file  */
    path = __flexiblas_mgmt_getenv(FLEXIBLAS_ENV_USER_RC);
    ini = (csc_ini_file_t *) config->user_config;
    csc_ini_empty(ini);
    if ( __flexiblas_file_exist(path)) {
        DPRINTF(1, "Load user config %s\n", path);
        csc_ini_load(path, ini, CSC_INI_LOAD_SECTION_UPPERCASE);
    } else {
        DPRINTF_WARN(1, "Config %s does not exist.\n", path);
    }

    if ( path ) free(path);

    /* Host file  */
    path = __flexiblas_mgmt_getenv(FLEXIBLAS_ENV_HOST_RC);
    ini = (csc_ini_file_t *) config->host_config;
    csc_ini_empty(ini);
    if ( __flexiblas_file_exist(path)) {
        DPRINTF(1, "Load host config %s\n", path);
        csc_ini_load(path, ini, CSC_INI_LOAD_SECTION_UPPERCASE);
    } else {
        DPRINTF_WARN(1, "Config %s does not exist.\n", path);
    }

    if ( path ) free(path);

    /* Enviroment supplied config file  */
    path = __flexiblas_mgmt_getenv(FLEXIBLAS_ENV_ENV_RC);
    ini = (csc_ini_file_t *) config->env_config;
    csc_ini_empty(ini);
    if ( __flexiblas_file_exist(path)) {
        DPRINTF(1, "Load enviroment config %s\n", path);
        csc_ini_load(path, ini, CSC_INI_LOAD_SECTION_UPPERCASE);
    } else {
        DPRINTF_WARN(1, "Environment supplied config (%s) does not exist.\n", path);
    }

    if ( path ) free(path);


    flexiblas_mgmt_update_name_list(config);
    return config;

}

/*  Update available BLAS names */
int flexiblas_mgmt_update_name_list(flexiblas_mgmt_t *config)
{
    csc_ini_section_t *sec;
    csc_ini_file_t *ini;
    void *iter = NULL;
    int i,j;

    if ( config == NULL) return -1;

    if (config->nblas_names > 0 ){
        size_t p;
        for (p = 0; p < config->nblas_names; p++) {
            free(config->blas_names[p]);
        }
        free(config->blas_names);
        config->blas_names = NULL;
        config->nblas_names = 0;
    }

    for (i = 0; i < FLEXIBLAS_MGMT_LOCATION_COUNT; i++) {
        ini = idx_to_ini(i, config);

        iter = NULL;
        while ( (sec = csc_ini_section_iterator( ini , &iter)) != NULL){
            char *sec_name  = csc_ini_getsectionname(sec);
            int pos = 0;
            int found  = 0;

            if (sec_name == CSC_INI_DEFAULT_SECTION)
                continue;

            if (csc_strcasebegin(sec_name, "HOOK-") != 0 ){
                continue;
            } else {
                pos = config->nblas_names;
                /* Check if exists */
                for (j = 0; j < pos; j++) {
                    if (csc_strcasecmp(config->blas_names[j], sec_name) == 0) {
                        found = 1;
                        break;
                    }
                }

                if ( !found) {
                    config->nblas_names++;
                    config->blas_names=realloc(config->blas_names, (pos+1)*sizeof(char*));
                    config->blas_names[pos] = strdup(sec_name);
                    config->blas_names[pos] = csc_struppercase(config->blas_names[pos]);
                }
            }
        }
    }
    return 0;
}

/* Write the configuration to a given location.
 * The system config directories cannot be written. */
int flexiblas_mgmt_write_config2(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc)
{
    csc_ini_file_t * ini;
    char *write_name = NULL;
    int ret = 0;

    if ( loc == FLEXIBLAS_GLOBAL) {
        ini = ( csc_ini_file_t *) config->system_config;
        write_name = __flexiblas_mgmt_getenv(FLEXIBLAS_ENV_GLOBAL_RC);
    } else if ( loc == FLEXIBLAS_USER ) {
        ini = ( csc_ini_file_t *) config->user_config;
        write_name = __flexiblas_mgmt_getenv(FLEXIBLAS_ENV_USER_RC);
    } else if ( loc == FLEXIBLAS_HOST ) {
        ini = ( csc_ini_file_t *) config->host_config;
        write_name = __flexiblas_mgmt_getenv(FLEXIBLAS_ENV_HOST_RC);
    } else if ( loc == FLEXIBLAS_ENV ) {
        ini = ( csc_ini_file_t *) config->env_config;
        write_name = __flexiblas_mgmt_getenv(FLEXIBLAS_ENV_ENV_RC);
    } else {
        return -1;
    }

    if ( write_name ) {
        if ( csc_ini_has_changed(ini) > 0 ) {
            if ( csc_ini_write(write_name, ini) != CSC_INI_SUCCESS ) {
                DPRINTF_ERROR(0, "Writing to %s failed.\n", write_name);
                ret = -2;
            }
        } else {
            DPRINTF(2, "Configuration file %s unchanged.\n", write_name);
        }
        free(write_name);
    }

    return ret;
}

/* Write all configs, except of the system config directories.  */
int flexiblas_mgmt_write_config(flexiblas_mgmt_t *config)
{
    int ret = 0;

    if ( flexiblas_mgmt_write_config2(config, FLEXIBLAS_GLOBAL))
        ret |= 0x01;
    if ( flexiblas_mgmt_write_config2(config, FLEXIBLAS_USER))
        ret |= 0x02;
    if ( flexiblas_mgmt_write_config2(config, FLEXIBLAS_HOST))
        ret |= 0x04;
    if ( flexiblas_mgmt_write_config2(config, FLEXIBLAS_ENV))
        ret |= 0x08;

    return ret;
}

/* Free the config */
void flexiblas_mgmt_free_config(flexiblas_mgmt_t *config)
{
    if ( config == NULL) return;
    if ( config->system_config != NULL) {
        csc_ini_free((csc_ini_file_t *)config->system_config);
        free(config->system_config);
    }

    if ( config->system_dir_config != NULL) {
        csc_ini_free((csc_ini_file_t *)config->system_dir_config);
        free(config->system_dir_config);
    }

    if ( config->user_config != NULL) {
        csc_ini_free((csc_ini_file_t *)config->user_config);
        free(config->user_config);
    }
    if ( config->host_config != NULL) {
        csc_ini_free((csc_ini_file_t *)config->host_config);
        free(config->host_config);
    }

    if ( config->env_config != NULL) {
        csc_ini_free((csc_ini_file_t *)config->env_config);
        free(config->env_config);
    }

    if ( config->nblas_names > 0 ) {
        size_t p;
        for (p = 0; p < config->nblas_names; p++) {
            free(config->blas_names[p]);
        }
        free(config->blas_names);
    }

    free(config);
    return;
}



/*-----------------------------------------------------------------------------
 *  List BLAS libraries
 *-----------------------------------------------------------------------------*/
int flexiblas_mgmt_list_blas(flexiblas_mgmt_t * config, flexiblas_mgmt_location_t loc,
        char *blas_name, char * library, char *comment, void **help)
{
    csc_ini_iterator_t iter;
    csc_ini_section_t *sec;
    char *tmp;

    if ( config == NULL ) {
        return -1;
    }
    iter = *help;

iter_start:
    if (loc == FLEXIBLAS_GLOBAL ){
        sec = csc_ini_section_iterator((csc_ini_file_t *) config->system_config, &iter);
    } else if ( loc == FLEXIBLAS_GLOBAL_DIR ) {
        sec = csc_ini_section_iterator((csc_ini_file_t *) config->system_dir_config, &iter);
    } else if ( loc == FLEXIBLAS_USER ) {
        sec = csc_ini_section_iterator((csc_ini_file_t *) config->user_config, &iter);
    } else if ( loc == FLEXIBLAS_HOST ) {
        sec = csc_ini_section_iterator((csc_ini_file_t *) config->host_config, &iter);
    } else if ( loc == FLEXIBLAS_ENV ) {
        sec = csc_ini_section_iterator((csc_ini_file_t *) config->env_config, &iter);
    } else {
        return -1;
    }
    if (sec != NULL) {
        char *sec_name  = csc_ini_getsectionname(sec);
        if (sec_name == CSC_INI_DEFAULT_SECTION || csc_strcasebegin(sec_name,"HOOK-"))
            goto iter_start;
        else
            strncpy(blas_name, sec_name, FLEXIBLAS_MGMT_MAX_BUFFER_LEN);

        if ( (csc_ini_section_getstring(sec, "library", &tmp) != CSC_INI_SUCCESS) ) {
			strncpy(library, "", FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
            library[0] = 0;
		} else {
			strncpy(library, tmp, FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
		}

        if ( (csc_ini_section_getstring(sec, "comment", &tmp) != CSC_INI_SUCCESS) ) {
			strncpy(comment, "", FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
            comment[0] = 0;
		} else {
			strncpy(comment, tmp, FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
		}
        *help = iter;
        return 1;
    } else {
        *help = NULL;
        return 0;
    }
}


/*-----------------------------------------------------------------------------
 *  List Paths
 *-----------------------------------------------------------------------------*/
int flexiblas_mgmt_list_paths(flexiblas_mgmt_t * config, flexiblas_mgmt_location_t loc, char *path, void **help)
{
    csc_ini_iterator_t iter;
    csc_ini_section_t *sec;
	csc_ini_kvstore_t  *kv;

    iter = * help;

    if ( config == NULL)
        return -1;
    if ( iter == NULL ) {
        if (loc == FLEXIBLAS_GLOBAL ){
            sec = csc_ini_getsection((csc_ini_file_t *) config->system_config, CSC_INI_DEFAULT_SECTION);
        } else if ( loc == FLEXIBLAS_GLOBAL_DIR ) {
            sec = csc_ini_getsection((csc_ini_file_t *) config->system_dir_config, CSC_INI_DEFAULT_SECTION );
        } else if ( loc == FLEXIBLAS_USER ) {
            sec = csc_ini_getsection((csc_ini_file_t *) config->user_config, CSC_INI_DEFAULT_SECTION );
        } else if ( loc == FLEXIBLAS_HOST ) {
            sec = csc_ini_getsection((csc_ini_file_t *) config->host_config, CSC_INI_DEFAULT_SECTION);
        } else if ( loc == FLEXIBLAS_ENV ) {
            sec = csc_ini_getsection((csc_ini_file_t *) config->env_config, CSC_INI_DEFAULT_SECTION);
        } else {
            return -1;
        }
        if ( sec == NULL )
            return -2;
    }

iter:
    kv = csc_ini_kvstore_iterator(sec, &iter);
    if (kv == NULL) return 0;
    char *key = csc_ini_getkey(kv);

    if ( csc_strbegin(key, "path")) {
        strncpy(path, csc_ini_getvalue(kv), FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
	} else {
        goto iter;
    }
    *help = iter;
    return 1;
}


static char flexiblas_mgmt_searchpath[] = FLEXIBLAS_DEFAULT_LIB_PATH;

int flexiblas_mgmt_list_default_paths(char *path, void **help)
{
	char *r;
    // printf("%s\n", flexiblas_mgmt_searchpath);
    if ( *help == NULL ) {
        strncpy(path, strtok_r(flexiblas_mgmt_searchpath, ":",&r), FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
        *help = r;
        return 1;
    } else {
        char *p;
        r = (char *) *help ;
        p = strtok_r(NULL, ":",&r);
        if ( p == NULL ) {
            *help =  NULL;
            return 0;
        }
        *help =r;
        strncpy(path,  p , FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
        return 1;

    }
    return -1;

}


/*-----------------------------------------------------------------------------
 *  Manage Defaults
 *-----------------------------------------------------------------------------*/
int flexiblas_mgmt_get_default(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc,  char *def )
{
    csc_ini_file_t *ini;
    char *def_load;

    if ( config == NULL ){
        return -1;
    }
    ini = loc_to_ini(loc, config);

    if ( ini == NULL) {
        strncpy(def, "(none)", FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
        return 1;
    }

    if ( csc_ini_getstring( ini , CSC_INI_DEFAULT_SECTION, "default", &def_load)
            != CSC_INI_SUCCESS) {
        strncpy(def, "(none)", FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
        return 2;
    }
    strncpy(def, def_load, FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
    csc_struppercase(def);
    return 0;
}

/*
 * System config directories cannot include a default setting.
 * */
int flexiblas_mgmt_get_active_default(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t *loc , char *def )
{
    int ret_system_default, ret_user_default, ret_host_default, ret_env_default;

    ret_system_default = flexiblas_mgmt_get_default(config, FLEXIBLAS_GLOBAL, def);
    ret_user_default =   flexiblas_mgmt_get_default(config, FLEXIBLAS_USER, def);
    ret_host_default =   flexiblas_mgmt_get_default(config, FLEXIBLAS_HOST, def);
    ret_env_default  =   flexiblas_mgmt_get_default(config, FLEXIBLAS_ENV, def);

    if ( ret_env_default == 0 ) {
        *loc = FLEXIBLAS_ENV;
        flexiblas_mgmt_get_default(config, FLEXIBLAS_ENV, def);
        return 0;
    } else if ( ret_host_default == 0 ) {
        *loc = FLEXIBLAS_HOST;
        flexiblas_mgmt_get_default(config, FLEXIBLAS_HOST, def);
        return 0;
    } else if (ret_user_default == 0 ) {
        *loc = FLEXIBLAS_USER;
        flexiblas_mgmt_get_default(config, FLEXIBLAS_USER, def);
        return 0;
    } else if (ret_system_default == 0) {
        *loc = FLEXIBLAS_GLOBAL;
        flexiblas_mgmt_get_default(config, FLEXIBLAS_GLOBAL, def);
        return 0;
    } else {
        *loc = FLEXIBLAS_DEFAULT;
        /* Use the compile-time default.  */
        strncpy(def, FLEXIBLAS_DEFAULT_BLAS , FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
        return 0;
	}

    return -1;
}

int flexiblas_mgmt_blas_exists(flexiblas_mgmt_t *config, char* blas_name, flexiblas_mgmt_location_t *loc)
{
    csc_ini_file_t *ini;
    csc_ini_section_t *sec;
    char *iname = NULL;
    int i;
    if ( config == NULL ){
        return 0;
    }

    iname = strdup(blas_name);
    if (!iname) return 0;
    iname = csc_struppercase(iname);
    for (i = 0; i < FLEXIBLAS_MGMT_LOCATION_COUNT; i++) {
        ini = idx_to_ini(i, config);

        sec = csc_ini_getsection(ini, iname);
        if ( sec != NULL ) {
            if ( loc != NULL) {
                if ( i == 0) *loc = FLEXIBLAS_GLOBAL;
                else if ( i == 1 ) *loc = FLEXIBLAS_GLOBAL_DIR;
                else if ( i == 2 ) *loc = FLEXIBLAS_USER;
                else if ( i == 3 ) *loc = FLEXIBLAS_HOST;
                else if ( i == 4 ) *loc = FLEXIBLAS_ENV;
            }
            free(iname);
            return 1;
        }
    }
    free(iname);
    return 0;
}

int flexiblas_mgmt_set_default(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc,  char *def )
{
    csc_ini_file_t * ini;
    csc_ini_error_t ret = CSC_INI_SUCCESS;

    ini = loc_to_ini(loc, config);
    if ( ini == NULL ) return -1;
    /* Default cannot be set in system config directories. */
    if ( loc == FLEXIBLAS_GLOBAL_DIR) return -1;

    if (def == NULL ){
        ret = csc_ini_key_remove(ini, CSC_INI_DEFAULT_SECTION, "default");
		if ( ret != CSC_INI_NOSECTION && ret != CSC_INI_NOKEY && ret != CSC_INI_SUCCESS){
			printf("Failed to delete default BLAS from %s. Exit\n", flexiblas_mgmt_location_to_string(loc));
            return -1;
		}
        return 0;
    }

    if ( flexiblas_mgmt_blas_exists(config, def, NULL)) {
        char *def2 = strdup(def);
        def2 = csc_struppercase(def2);
        if ( csc_ini_setstring(ini, CSC_INI_DEFAULT_SECTION, "default", def2) != CSC_INI_SUCCESS) {
            free(def2);
			printf("Failed to set default to %s. Exit\n", def);
            return -1;
		}
        free(def2);
        return 0;
    }
    return -1;
}

int flexiblas_mgmt_blas_get(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc,
        const char *blas_name, char *library, char *comment)
{
    csc_ini_file_t *ini;
    char *iname = NULL;
    csc_ini_section_t *sec;
    char *tmp = NULL;

    ini = loc_to_ini(loc, config);
    if ( ini == NULL ) return -1;

    iname = strdup(blas_name);
    if (!iname) return -1;
    iname  = csc_struppercase(iname);

    sec = csc_ini_getsection(ini, iname);
    free(iname);
    if ( sec == NULL) {
        return -1;
    } else {
        if ( (csc_ini_section_getstring(sec, "library", &tmp) != CSC_INI_SUCCESS) ) {
			strncpy(library, "", FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
            library[0] = 0;
		} else {
			strncpy(library, tmp, FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
		}

        if ( comment != NULL) {
            if ( (csc_ini_section_getstring(sec, "comment", &tmp) != CSC_INI_SUCCESS) ) {
                strncpy(comment, "", FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
                comment[0] = 0;
            } else {
                strncpy(comment, tmp, FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
            }
        }
        return 0;
    }
    return 0;
}

int flexiblas_mgmt_blas_get2(flexiblas_mgmt_t *config,  flexiblas_mgmt_location_t *loc2, const char *blas_name, char *library, char *comment)
{
    int i = 0;
    int found  = 0;
    flexiblas_mgmt_location_t loc;

    for (i = 0; i < FLEXIBLAS_MGMT_LOCATION_COUNT; i++) {
        if ( i == 0) loc = FLEXIBLAS_GLOBAL;
        else if ( i == 1 ) loc = FLEXIBLAS_GLOBAL_DIR;
        else if ( i == 2 ) loc = FLEXIBLAS_USER;
        else if ( i == 3 ) loc = FLEXIBLAS_HOST;
        else if ( i == 4 ) loc = FLEXIBLAS_ENV;

        if ( flexiblas_mgmt_blas_get(config, loc, blas_name, library, comment) == 0) {
            found =1;
            *loc2 = loc;
            break;
        }
    }
    if ( found)
        return 0;
    else
        return -1;
}

int flexiblas_mgmt_blas_add (flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc,
            char *name, char *so_name, char*comment)
{
	char *iname = NULL;
    csc_ini_error_t ret;
    csc_ini_file_t *ini;

    if (config==NULL) {
        return -1;
    }

    ini = loc_to_ini(loc, config);
    if ( ini == NULL ) return -1;

    /* System config directories cannot be used. */
    if ( loc == FLEXIBLAS_GLOBAL_DIR) return -1;

    iname = strdup(name);
	iname = csc_struppercase(iname);

    ret = csc_ini_setstring(ini, iname, "library", so_name);
	if ( ret != CSC_INI_SUCCESS ) {
		printf("Failed to set the library entry for %s. Exit.\n", iname);
        free(iname);
        return -1;
	}

    if ( comment != NULL) {
		ret = csc_ini_setstring(ini, iname, "comment", comment);
		if ( ret != CSC_INI_SUCCESS) {
			printf("Failed to set the comment for %s. Exit.\n", iname);
            free(iname);
            return -1;
		}
	}
    free(iname);
    flexiblas_mgmt_update_name_list(config);

    return 0;
}

int flexiblas_mgmt_blas_remove(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc, char *name)
{
	char *iname = NULL;
    csc_ini_error_t ret;
    csc_ini_file_t *ini;

    if (config==NULL) {
        return -1;
    }

    ini = loc_to_ini(loc, config);
    if ( ini == NULL ) return -1;
    /* System config directories cannot be used. */
    if ( loc == FLEXIBLAS_GLOBAL_DIR) return -1;


    iname = strdup(name);
	iname = csc_struppercase(iname);
    ret = csc_ini_section_remove(ini, iname);
    if ( ret!=CSC_INI_SUCCESS) {
        printf("Failed to remove %s from %s.\n", name, flexiblas_mgmt_location_to_string(loc));
        return -1;
    }

    /*-----------------------------------------------------------------------------
     *  Detect if the default is still working
     *-----------------------------------------------------------------------------*/
    char def_blas[FLEXIBLAS_MGMT_MAX_BUFFER_LEN];
    def_blas[0] = 0;

    /* System   */
    flexiblas_mgmt_get_default(config, FLEXIBLAS_GLOBAL, def_blas);
    if ( csc_strcasecmp(def_blas, name) == 0 ){
        flexiblas_mgmt_set_default(config, FLEXIBLAS_GLOBAL, NULL);
    }
    def_blas[0] = 0;

    /* User   */
    flexiblas_mgmt_get_default(config, FLEXIBLAS_USER, def_blas);
    if ( csc_strcasecmp(def_blas, name) == 0 ){
        flexiblas_mgmt_set_default(config, FLEXIBLAS_USER, NULL);
    }
    def_blas[0] = 0;

    /* Host   */
    flexiblas_mgmt_get_default(config, FLEXIBLAS_HOST, def_blas);
    if ( csc_strcasecmp(def_blas, name) == 0 ){
        flexiblas_mgmt_set_default(config, FLEXIBLAS_HOST, NULL);
    }
    def_blas[0] = 0;

    /* Env   */
    flexiblas_mgmt_get_default(config, FLEXIBLAS_ENV, def_blas);
    if ( csc_strcasecmp(def_blas, name) == 0 ){
        flexiblas_mgmt_set_default(config, FLEXIBLAS_ENV, NULL);
    }
    def_blas[0] = 0;

    return 0;
}

/*-----------------------------------------------------------------------------
 *  Manage Properties
 *-----------------------------------------------------------------------------*/
int flexiblas_mgmt_get_property(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc,
        flexiblas_mgmt_property_t prop, void *buffer)
{
    int integer = 0;
    csc_ini_file_t *ini;
    if (config == NULL)
        return -1;

    ini = loc_to_ini(loc, config);
    if ( ini == NULL ) return -1;
    /* System config directories cannot be used. */
    if ( loc == FLEXIBLAS_GLOBAL_DIR) return -1;


    switch (prop) {
        case FLEXIBLAS_PROP_VERBOSE:
            if ( csc_ini_getinteger( ini , CSC_INI_DEFAULT_SECTION, "verbose", &integer)
                    == CSC_INI_SUCCESS ){
                *((int*) buffer) = integer;
                return 0;
            }
            return -1;
        case FLEXIBLAS_PROP_NOLAPACK:
            if ( csc_ini_getinteger( ini , CSC_INI_DEFAULT_SECTION, "nolapack", &integer)
                    == CSC_INI_SUCCESS ){
                *((int*) buffer) = integer;
                return 0;
            }
            return -1;
        default:
            return -1;

    }
    return 0;
}

int flexiblas_mgmt_get_active_property(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t *loc,
            flexiblas_mgmt_property_t prop, void *buffer)
{
    int ret_system, ret_user, ret_host, ret_env;
    if ( config == NULL)
        return -1;

    /* System config directories cannot be used. */
    ret_system = flexiblas_mgmt_get_property(config, FLEXIBLAS_GLOBAL,  prop, buffer);
    ret_user   = flexiblas_mgmt_get_property(config, FLEXIBLAS_USER, prop, buffer);
    ret_host   = flexiblas_mgmt_get_property(config, FLEXIBLAS_HOST, prop, buffer);
    ret_env    = flexiblas_mgmt_get_property(config, FLEXIBLAS_ENV, prop, buffer);

    if ( ret_env == 0 ) {
        /* Host  */
        *loc = FLEXIBLAS_ENV;
        flexiblas_mgmt_get_property(config, FLEXIBLAS_ENV, prop, buffer);
        return 0;
    } else if ( ret_host == 0 ) {
        /* Host  */
        *loc = FLEXIBLAS_HOST;
        flexiblas_mgmt_get_property(config, FLEXIBLAS_HOST, prop, buffer);
        return 0;
    } else if ( ret_user == 0 ) {
        /* User */
        *loc = FLEXIBLAS_USER;
        flexiblas_mgmt_get_property(config, FLEXIBLAS_USER, prop, buffer);
        return 0;
    } else if ( ret_system == 0 ) {
        *loc = FLEXIBLAS_GLOBAL;
        flexiblas_mgmt_get_property(config, FLEXIBLAS_GLOBAL,  prop, buffer);
        return 0;
    } else {
        /*  Default  */
        flexiblas_mgmt_default_property(prop, buffer);
    }
    return 0;
}

int flexiblas_mgmt_set_property(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc,
        flexiblas_mgmt_property_t prop, void *buffer)
{
    csc_ini_file_t *ini;
    ini = loc_to_ini(loc, config);
    if ( ini == NULL ) return -1;

    /* System config directories cannot be used. */
    if ( loc == FLEXIBLAS_GLOBAL_DIR) return -1;

    if (buffer == NULL) {
        switch(prop) {
            case FLEXIBLAS_PROP_VERBOSE:
                csc_ini_key_remove(ini, CSC_INI_DEFAULT_SECTION, "verbose");
                break;
            case FLEXIBLAS_PROP_NOLAPACK:
                csc_ini_key_remove(ini, CSC_INI_DEFAULT_SECTION, "nolapack");
                break;

            default:
                break;
        }
    } else {
         switch(prop) {
            case FLEXIBLAS_PROP_VERBOSE:
                csc_ini_setinteger(ini, CSC_INI_DEFAULT_SECTION, "verbose", *((int *) buffer));
                break;
            case FLEXIBLAS_PROP_NOLAPACK:
                csc_ini_setinteger(ini, CSC_INI_DEFAULT_SECTION, "nolapack", *((int *) buffer));
                break;

            default:
                break;
        }

    }
    return -1;
}



int flexiblas_mgmt_default_property(flexiblas_mgmt_property_t prop, void *buffer)
{
    switch (prop) {
        case FLEXIBLAS_PROP_VERBOSE:
            *((int*) buffer) = 0;
            break;
        case FLEXIBLAS_PROP_NOLAPACK:
            *((int*) buffer) = 0;
            break;

        default:
            return -1;

    }
    return 0;
}


/* **************************************************
 * Manage Keys
 **/
int flexiblas_mgmt_get_key(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc,
        char * section, char *key, char *buffer)
{
    csc_ini_file_t *ini;
    char *str = NULL;
    if (config == NULL)
        return -1;

    ini = loc_to_ini(loc, config);
    if ( ini == NULL ) return -1;

    if ( csc_ini_getstring( ini , section, key, &str)
            == CSC_INI_SUCCESS ){
        strncpy((char*) buffer, str, FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
        return 0;
    }
    return -1;
}



int flexiblas_mgmt_get_active_key(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t *loc,
            char *section, char *key, char*buffer)
{
    int ret_system, ret_user, ret_host, ret_env;
    if ( config == NULL)
        return -1;

    /* System config directories cannot be used. */
    ret_system = flexiblas_mgmt_get_key(config, FLEXIBLAS_GLOBAL, section, key, buffer);
    ret_user   = flexiblas_mgmt_get_key(config, FLEXIBLAS_USER,   section, key, buffer);
    ret_host   = flexiblas_mgmt_get_key(config, FLEXIBLAS_HOST,   section, key, buffer);
    ret_env    = flexiblas_mgmt_get_key(config, FLEXIBLAS_ENV,    section, key, buffer);

    if ( ret_env == 0 ) {
        /* Host  */
        *loc = FLEXIBLAS_ENV;
        flexiblas_mgmt_get_key(config, FLEXIBLAS_ENV, section, key, buffer);
        return 0;
    } else if ( ret_host == 0 ) {
        /* Host  */
        *loc = FLEXIBLAS_HOST;
        flexiblas_mgmt_get_key(config, FLEXIBLAS_HOST, section, key, buffer);
        return 0;
    } else if ( ret_user == 0 ) {
        /* User */
        *loc = FLEXIBLAS_USER;
        flexiblas_mgmt_get_key(config, FLEXIBLAS_USER, section, key, buffer);
        return 0;
    } else if ( ret_system == 0 ) {
        *loc = FLEXIBLAS_GLOBAL;
        flexiblas_mgmt_get_key(config, FLEXIBLAS_GLOBAL, section, key, buffer);
        return 0;
    } else {
        /*  Default  */
        buffer[0] = '\0';
        return -2;
    }
    return 0;
}






