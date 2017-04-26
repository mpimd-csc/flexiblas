/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Kohler, 2016
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>

#include "flexiblas.h"
#include "flexiblas_mgmt.h"
#include "cscutils/strutils.h"

#define MAX_BUFFER_SIZE 32*1024

/*  Internal uppercase funtion */ 
static char *uppercase(char *str) {
	char *ret = str; 
	if ( str == NULL ) return NULL; 
	while (*str != '\0') {
		*str = toupper(*str); 
		str++; 		
	}
	return ret; 
}

/*  Internal wrapper around getemv  */
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
                char hostname[MAX_BUFFER_SIZE];
                gethostname(hostname, MAX_BUFFER_SIZE);
    			snprintf(container,MAX_BUFFER_SIZE,"%s/.%s.%s", getenv("HOME"), FLEXIBLAS_RC, hostname);
            }
			#endif
			break;


		default:
			return NULL;
	}
	return strdup(container);
}
#undef MAX_BUFFER_SIZE

/*  Return the location of the config file. */
char *flexiblas_mgmt_location(flexiblas_mgmt_location_t loc)
{
    if ( loc == FLEXIBLAS_GLOBAL ){
        return  __flexiblas_mgmt_getenv(FLEXIBLAS_ENV_GLOBAL_RC);

    } else if ( loc == FLEXIBLAS_USER ) {
        return __flexiblas_mgmt_getenv(FLEXIBLAS_ENV_USER_RC);
    } else if (loc == FLEXIBLAS_HOST ) {
    	return __flexiblas_mgmt_getenv(FLEXIBLAS_ENV_HOST_RC);
    }
    return NULL;
}

/*  Create a human reable string from the config location enum. */
char *flexiblas_mgmt_location_to_string(flexiblas_mgmt_location_t loc){
    static char *system_str = "System";
    static char *user_str   = "User";
    static char *host_str   = "Host";
    static char *default_str = "Compiled-In Default"; 
    static char *lz = "";
    if ( loc == FLEXIBLAS_GLOBAL ) 
        return system_str;
    if ( loc == FLEXIBLAS_USER ) 
        return user_str;
    if ( loc == FLEXIBLAS_HOST) 
        return host_str;
    if ( loc == FLEXIBLAS_DEFAULT) 
        return default_str;
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
    config->user_config = (csc_ini_file_t *) malloc(sizeof(csc_ini_file_t) * (1));
    config->host_config = (csc_ini_file_t *) malloc(sizeof(csc_ini_file_t) * (1));

    if ( config->system_config == NULL
            || config->user_config == NULL 
            || config->host_config == NULL ) 
    {
        flexiblas_mgmt_free_config(config);
        return NULL;
    }

    /*-----------------------------------------------------------------------------
     *  Load
     *-----------------------------------------------------------------------------*/
     /*  System file  */
    path = __flexiblas_mgmt_getenv(FLEXIBLAS_ENV_GLOBAL_RC);
    ini = (csc_ini_file_t *) config->system_config; 
    csc_ini_empty(ini);
    csc_ini_load(path, ini, CSC_INI_LOAD_SECTION_UPPERCASE);
    free(path);

    /* User file  */
    path = __flexiblas_mgmt_getenv(FLEXIBLAS_ENV_USER_RC);
    ini = (csc_ini_file_t *) config->user_config; 
    csc_ini_empty(ini);
    csc_ini_load(path, ini, CSC_INI_LOAD_SECTION_UPPERCASE);
    free(path);

    /* Host file  */
    path = __flexiblas_mgmt_getenv(FLEXIBLAS_ENV_HOST_RC);
    ini = (csc_ini_file_t *) config->host_config; 
    csc_ini_empty(ini);
    csc_ini_load(path, ini, CSC_INI_LOAD_SECTION_UPPERCASE);
    free(path);

    flexiblas_mgmt_update_name_list(config);
    return config;
    
}

/*  Update available BLAS names */
int flexiblas_mgmt_update_name_list(flexiblas_mgmt_t *config) 
{
    csc_ini_section_t *sec;
    csc_ini_file_t *ini;
    void *iter = NULL;
    int i;

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

    for (i = 0; i < 3; i++) {
        if ( i == 0 ) 
            ini = (csc_ini_file_t *) config->system_config;
        else if (i==1) {
            ini = (csc_ini_file_t *) config->user_config;
        } else if (i==2) {
            ini = (csc_ini_file_t *) config->host_config;
        }

        iter = NULL;
        while ( (sec = csc_ini_section_iterator( ini , &iter)) != NULL){
            char *sec_name  = csc_ini_getsectionname(sec);
            int pos = 0;
            int found  = 0;

            if (sec_name == CSC_INI_DEFAULT_SECTION)
                continue;

            pos = config->nblas_names;
            /* Check if exists */
            for (i = 0; i < pos; i++) {
                if (csc_strcasecmp(config->blas_names[i], sec_name) == 0) {
                    found = 1;
                    break;
                }
            }

            if ( !found) {
                config->nblas_names++;
                config->blas_names=realloc(config->blas_names, (pos+1)*sizeof(char*));
                config->blas_names[pos] = strdup(sec_name);
                config->blas_names[pos] = uppercase(config->blas_names[pos]);
            }
        }
    }
    return 0;
}

/* Write the configuration to a given location  */
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
    } else {
        return -1;
    }

    if ( csc_ini_write(write_name, ini) != CSC_INI_SUCCESS ) {
        DPRINTF(0, COLOR_RED "Writing to %s failed.\n" COLOR_RESET, write_name);
        ret = -2;
    }
    free(write_name);

    return ret;
}

/* Write all configs  */
int flexiblas_mgmt_write_config(flexiblas_mgmt_t *config)
{
    int ret = 0;

    if ( flexiblas_mgmt_write_config2(config, FLEXIBLAS_GLOBAL))
        ret |= 0x01;
    if ( flexiblas_mgmt_write_config2(config, FLEXIBLAS_USER))
        ret |= 0x02;
    if ( flexiblas_mgmt_write_config2(config, FLEXIBLAS_HOST))
        ret |= 0x04;
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
    if ( config->user_config != NULL) {
        csc_ini_free((csc_ini_file_t *)config->user_config);
        free(config->user_config);
    }
    if ( config->host_config != NULL) {
        csc_ini_free((csc_ini_file_t *)config->host_config);
        free(config->host_config);
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
    } else if ( loc == FLEXIBLAS_USER ) {
        sec = csc_ini_section_iterator((csc_ini_file_t *) config->user_config, &iter);
    } else if ( loc == FLEXIBLAS_HOST ) {
        sec = csc_ini_section_iterator((csc_ini_file_t *) config->host_config, &iter);
    } else {
        return -1;
    }
    if (sec != NULL) {
        char *sec_name  = csc_ini_getsectionname(sec);
        if (sec_name == CSC_INI_DEFAULT_SECTION)
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
    if (loc == FLEXIBLAS_GLOBAL ){
        sec = csc_ini_section_iterator((csc_ini_file_t *) config->system_config, &iter);
    } else if ( loc == FLEXIBLAS_USER ) {
        sec = csc_ini_section_iterator((csc_ini_file_t *) config->user_config, &iter);
    } else if ( loc == FLEXIBLAS_HOST ) {
        sec = csc_ini_section_iterator((csc_ini_file_t *) config->host_config, &iter);
    } else {
        return -1;
    }

    if ( sec == NULL )
        return -2;
    
    kv = csc_ini_kvstore_iterator(sec, &iter);
    if (kv == NULL) return 0;
    char *key = csc_ini_getkey(kv); 
    
    if ( key[0] == 'p' && key[1]=='a' && key[2]=='t' && key[3] == 'h'){
        strncpy(path, csc_ini_getvalue(kv), FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
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

    if ( loc == FLEXIBLAS_GLOBAL) {
        ini = ( csc_ini_file_t *) config->system_config;
    } else if ( loc == FLEXIBLAS_USER ) {
        ini = ( csc_ini_file_t *) config->user_config;
    } else if ( loc == FLEXIBLAS_HOST ) {
        ini = ( csc_ini_file_t *) config->host_config;
    } else {
        return -1;
    }

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
    uppercase(def); 
    return 0;
}

int flexiblas_mgmt_get_active_default(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t *loc , char *def )
{
    int ret_system_default, ret_user_default, ret_host_default;
    
    ret_system_default = flexiblas_mgmt_get_default(config, FLEXIBLAS_GLOBAL, def);
    ret_user_default =   flexiblas_mgmt_get_default(config, FLEXIBLAS_USER, def);
    ret_host_default =   flexiblas_mgmt_get_default(config, FLEXIBLAS_HOST, def);

    if ( ret_host_default == 0 ) {
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
        strncpy(def, "NETLIB", FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
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
    iname = uppercase(iname);
    for (i = 0; i < 3; i++) {
        if ( i == 0) 
            ini = ( csc_ini_file_t *) config->host_config;
        else if ( i == 1 ) 
            ini = ( csc_ini_file_t *) config->user_config;
        else if ( i == 2 ) 
            ini = ( csc_ini_file_t *) config->system_config;
        else 
            ini = NULL;

        sec = csc_ini_getsection(ini, iname);
        if ( sec != NULL ) {
            if ( loc != NULL) {
                if ( i == 0) *loc = FLEXIBLAS_HOST;
                else if ( i == 1 ) *loc = FLEXIBLAS_USER;
                else if ( i == 2 ) *loc = FLEXIBLAS_GLOBAL;
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

    if ( loc == FLEXIBLAS_GLOBAL) {
        ini = ( csc_ini_file_t *) config->system_config;
    } else if ( loc == FLEXIBLAS_USER ) {
        ini = ( csc_ini_file_t *) config->user_config;
    } else if ( loc == FLEXIBLAS_HOST ) {
        ini = ( csc_ini_file_t *) config->host_config;
    } else {
        return -1;
    }

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
        def2 = uppercase(def2); 
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

    if ( loc == FLEXIBLAS_GLOBAL) {
        ini = ( csc_ini_file_t *) config->system_config;
    } else if ( loc == FLEXIBLAS_USER ) {
        ini = ( csc_ini_file_t *) config->user_config;
    } else if ( loc == FLEXIBLAS_HOST ) {
        ini = ( csc_ini_file_t *) config->host_config;
    } else {
        return -1;
    }

    iname = strdup(blas_name);
    if (!iname) return -1;
    iname  = uppercase(iname);

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

    for (i = 0; i < 3; i++) {
        if ( i == 0 )
            loc = FLEXIBLAS_HOST;
        else if ( i == 1)
            loc = FLEXIBLAS_USER;
        else if ( i == 2) 
            loc = FLEXIBLAS_GLOBAL;

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

    if ( loc == FLEXIBLAS_GLOBAL) {
        ini = ( csc_ini_file_t *) config->system_config;
    } else if ( loc == FLEXIBLAS_USER ) {
        ini = ( csc_ini_file_t *) config->user_config;
    } else if ( loc == FLEXIBLAS_HOST ) {
        ini = ( csc_ini_file_t *) config->host_config;
    } else {
        return -1;
    }

    iname = strdup(name); 
	iname = uppercase(iname); 

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

    if ( loc == FLEXIBLAS_GLOBAL) {
        ini = ( csc_ini_file_t *) config->system_config;
    } else if ( loc == FLEXIBLAS_USER ) {
        ini = ( csc_ini_file_t *) config->user_config;
    } else if ( loc == FLEXIBLAS_HOST ) {
        ini = ( csc_ini_file_t *) config->host_config;
    } else {
        return -1;
    }

    iname = strdup(name); 
	iname = uppercase(iname);
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
    char *str = NULL;
    if (config == NULL) 
        return -1;

    if ( loc == FLEXIBLAS_GLOBAL) {
        ini = ( csc_ini_file_t *) config->system_config;
    } else if ( loc == FLEXIBLAS_USER ) {
        ini = ( csc_ini_file_t *) config->user_config;
    } else if ( loc == FLEXIBLAS_HOST ) {
        ini = ( csc_ini_file_t *) config->host_config;
    } else {
        return -1;
    }

    switch (prop) {
        case FLEXIBLAS_PROP_VERBOSE:
            if ( csc_ini_getinteger( ini , CSC_INI_DEFAULT_SECTION, "verbose", &integer) 
                    == CSC_INI_SUCCESS ){
                *((int*) buffer) = integer;            
                return 0;
            }
            return -1;
         case FLEXIBLAS_PROP_PROFILE:
            if ( csc_ini_getinteger( ini , CSC_INI_DEFAULT_SECTION, "profile", &integer) 
                    == CSC_INI_SUCCESS ){
                *((int*) buffer) = integer;            
                return 0;
            }
            return -1;
        case FLEXIBLAS_PROP_PROFILE_FILE:
            if ( csc_ini_getstring( ini , CSC_INI_DEFAULT_SECTION, "profile_file", &str)
                    == CSC_INI_SUCCESS ){
                strncpy((char*) buffer, str, FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
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
    int ret_system, ret_user, ret_host;
    if ( config == NULL) 
        return -1;

    ret_system = flexiblas_mgmt_get_property(config, FLEXIBLAS_GLOBAL,  prop, buffer);
    ret_user   = flexiblas_mgmt_get_property(config, FLEXIBLAS_USER, prop, buffer);
    ret_host   = flexiblas_mgmt_get_property(config, FLEXIBLAS_HOST, prop, buffer);
    if ( ret_host == 0 ) {
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
     if ( loc == FLEXIBLAS_GLOBAL) {
        ini = ( csc_ini_file_t *) config->system_config;
    } else if ( loc == FLEXIBLAS_USER ) {
        ini = ( csc_ini_file_t *) config->user_config;
    } else if ( loc == FLEXIBLAS_HOST ) {
        ini = ( csc_ini_file_t *) config->host_config;
    } else {
        return -1;
    }

    if (buffer == NULL) {
        switch(prop) {
            case FLEXIBLAS_PROP_VERBOSE:
                csc_ini_key_remove(ini, CSC_INI_DEFAULT_SECTION, "verbose"); 
                break;
            case FLEXIBLAS_PROP_PROFILE:
                csc_ini_key_remove(ini, CSC_INI_DEFAULT_SECTION, "profile"); 
                break;
            case FLEXIBLAS_PROP_PROFILE_FILE:
                csc_ini_key_remove(ini, CSC_INI_DEFAULT_SECTION, "profile_file"); 
                break;
            default:
                break;
        } 
    } else {
         switch(prop) {
            case FLEXIBLAS_PROP_VERBOSE:
                csc_ini_setinteger(ini, CSC_INI_DEFAULT_SECTION, "verbose", *((int *) buffer)); 
                break;
            case FLEXIBLAS_PROP_PROFILE:
                csc_ini_setinteger(ini, CSC_INI_DEFAULT_SECTION, "profile", *((int *) buffer)); 
                break;
            case FLEXIBLAS_PROP_PROFILE_FILE:
                csc_ini_setstring(ini, CSC_INI_DEFAULT_SECTION, "profile_file", ((char *) buffer)); 
                break;
            default:
                break;
        } 

    }
    return -1;
}


static const char *__prop_profile_default = "stdout";

int flexiblas_mgmt_default_property(flexiblas_mgmt_property_t prop, void *buffer) 
{
    switch (prop) {
        case FLEXIBLAS_PROP_VERBOSE:
            *((int*) buffer) = 0;            
            break;
         case FLEXIBLAS_PROP_PROFILE:
            *((int*) buffer) = 0;            
            break;
        case FLEXIBLAS_PROP_PROFILE_FILE:
            strncpy((char*) buffer, __prop_profile_default, FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
            break;
        default:
            return -1;
            
    }
    return 0;
}

