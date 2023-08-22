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



#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>

#include "flexiblas.h"
#include "flexiblas_mgmt.h"
#include "hooks.h"
#include "helper.h"
#include "cscutils/strutils.h"

static char *__struppercase(char *str) {
	char *ret = str;
	if ( str == NULL ) return NULL;
	while (*str != '\0') {
		*str = toupper(*str);
		str++;
	}
	return ret;
}


/* Location to ini   */
static csc_ini_file_t * loc_to_ini (flexiblas_mgmt_location_t loc, flexiblas_mgmt_t *config) {
    csc_ini_file_t *ini;
    if ( loc == FLEXIBLAS_GLOBAL) {
        ini = ( csc_ini_file_t *) config->system_config;
    } else if ( loc == FLEXIBLAS_GLOBAL_DIR) {
        ini = ( csc_ini_file_t *) config->system_dir_config;
    } else if ( loc == FLEXIBLAS_USER ) {
        ini = ( csc_ini_file_t *) config->user_config;
    } else if ( loc == FLEXIBLAS_HOST ) {
        ini = ( csc_ini_file_t *) config->host_config;
    } else if ( loc == FLEXIBLAS_ENV ) {
        ini = ( csc_ini_file_t *) config->env_config;
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

static flexiblas_mgmt_location_t idx_to_loc(int i)
{
    flexiblas_mgmt_location_t loc ;
    if ( i == 0 )
        loc = FLEXIBLAS_GLOBAL;
    else if ( i == 1)
        loc = FLEXIBLAS_GLOBAL_DIR;
    else if ( i == 2)
        loc = FLEXIBLAS_USER;
    else if ( i == 3)
        loc = FLEXIBLAS_HOST;
    else if ( i == 4)
        loc = FLEXIBLAS_ENV;
    return loc;
}

/*
 *  Option management for hooks
 *
 */
int flexiblas_mgmt_hook_option_unset(flexiblas_mgmt_t * config, flexiblas_mgmt_location_t loc, char *hook, char *option)
{
    csc_ini_file_t *ini;
    char *iname = NULL;
    csc_ini_section_t *sec;
    size_t len;

    ini = loc_to_ini(loc, config);
    if ( ! ini ) return -1;

    len = 6 + strlen(hook);
    iname = malloc(sizeof(char *) * len);
    snprintf(iname, len, "HOOK-%s", hook);
	iname = csc_struppercase(iname);

    sec = csc_ini_getsection(ini, iname);
    free(iname);

    if ( sec == NULL) {
        return -2;
    } else {
        csc_ini_section_key_remove(sec, option);
    }
    return 0;
}

int flexiblas_mgmt_hook_option_get_int_loc(flexiblas_mgmt_t * config, flexiblas_mgmt_location_t loc, char * hook, char *option, int *val) {
    csc_ini_file_t *ini;
    char *iname = NULL;
    csc_ini_section_t *sec;
    int tmp = 0;
    size_t len;

    ini = loc_to_ini(loc, config);
    if ( ! ini ) return -1;

    len = 6 + strlen(hook);
    iname = malloc(sizeof(char *) * len);
    snprintf(iname, len, "HOOK-%s", hook);
	iname = csc_struppercase(iname);

    sec = csc_ini_getsection(ini, iname);
    free(iname);
    if ( sec == NULL) {
        return -1;
    } else {
        if ( (csc_ini_section_getinteger(sec, option, &tmp) != CSC_INI_SUCCESS) ) {
		    *val = 0;
            return -1;
        } else {
	        *val = tmp;
            return 0;
        }
    }
    return 0;
}

int flexiblas_mgmt_hook_option_get_string_loc(flexiblas_mgmt_t * config, flexiblas_mgmt_location_t loc, char * hook, char *option, char *str) {
    csc_ini_file_t *ini;
    char *iname = NULL;
    csc_ini_section_t *sec;
    char *tmp = NULL;
    size_t len;

    ini = loc_to_ini(loc, config );
    if ( ! ini ) return -1;

    len = 6 + strlen(hook);
    iname = malloc(sizeof(char *) * len);
    snprintf(iname, len, "HOOK-%s", hook);
	iname = csc_struppercase(iname);

    sec = csc_ini_getsection(ini, iname);
    free(iname);

    if ( sec == NULL) {
        return -1;
    } else {
        if ( (csc_ini_section_getstring(sec, option, &tmp) != CSC_INI_SUCCESS) ) {
			strncpy(str, "", FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
            str[0] = 0;
            return -1;
        } else {
			strncpy(str, tmp, FLEXIBLAS_MGMT_MAX_BUFFER_LEN);
            return 0;
		}
    }
    return 0;


}

int flexiblas_mgmt_hook_option_get_float_loc(flexiblas_mgmt_t * config, flexiblas_mgmt_location_t loc, char * hook, char *option, double *val) {
    csc_ini_file_t *ini;
    char *iname = NULL;
    csc_ini_section_t *sec;
    double tmp = 0;
    size_t len;

    ini = loc_to_ini(loc, config);
    if ( ! ini ) return -1;

    len = 6 + strlen(hook);
    iname = malloc(sizeof(char *) * len);
    snprintf(iname, len, "HOOK-%s", hook);
	iname = csc_struppercase(iname);

    sec = csc_ini_getsection(ini, iname);
    free(iname);

    if ( sec == NULL) {
        return -1;
    } else {
        if ( (csc_ini_section_getfloat(sec, option, &tmp) != CSC_INI_SUCCESS) ) {
		    *val = 0;
            return -1;
        } else {
	        *val = tmp;
            return 0;
        }
    }
    return 0;

}



int flexiblas_mgmt_hook_option_get_int(flexiblas_mgmt_t * config, char * hook, char *option, int *val) {
    int i = 0;
    int found  = 0;
    flexiblas_mgmt_location_t loc;

    for (i = 0; i < 4; i++) {
        loc = idx_to_loc(i);

        if ( flexiblas_mgmt_hook_option_get_int_loc(config, loc, hook, option, val) == 0) {
            found =1;
            break;
        }
    }
    if ( found)
        return 0;
    else
        return -1;
}

int flexiblas_mgmt_hook_option_get_string(flexiblas_mgmt_t * config, char * hook, char *option, char *str) {
    int i = 0;
    int found  = 0;
    flexiblas_mgmt_location_t loc;

    for (i = 0; i < 4; i++) {
        loc = idx_to_loc(i);

        if ( flexiblas_mgmt_hook_option_get_string_loc(config, loc, hook, option, str) == 0) {
            found =1;
            break;
        }
    }
    if ( found)
        return 0;
    else
        return -1;
}

int flexiblas_mgmt_hook_option_get_float(flexiblas_mgmt_t * config, char * hook, char *option, double *val) {
    int i = 0;
    int found  = 0;
    flexiblas_mgmt_location_t loc;

    for (i = 0; i < 4; i++) {
        loc = idx_to_loc(i);

        if ( flexiblas_mgmt_hook_option_get_float_loc(config, loc, hook, option, val) == 0) {
            found =1;
            break;
        }
    }
    if ( found)
        return 0;
    else
        return -1;
}


static int check_opt_name(char *name)
{
    if ( strcmp(name, "name") == 0 )
        return -1;
    if ( strcmp(name, "library") == 0 )
        return -1;
    return 0;
}

int flexiblas_mgmt_hook_option_set_int(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc,
        char *cfg_name, char *optname, int optval) {
    size_t len;
	char *iname = NULL;
    csc_ini_error_t ret;
    csc_ini_file_t *ini;

    if (config==NULL) {
        return -1;
    }

    if (check_opt_name(optname)) {
        DPRINTF_ERROR(0, "Option name \"%s\" is reserved for internal use.\n", optname);
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

    len = 6 + strlen(cfg_name);
    iname = malloc(sizeof(char *) * len);
    snprintf(iname, len, "HOOK-%s", cfg_name);
	iname = csc_struppercase(iname);

    ret = csc_ini_setinteger(ini, iname, optname, optval);
	if ( ret != CSC_INI_SUCCESS ) {
		printf("Failed to set the %s option entry for %s. Exit.\n", optname, cfg_name);
        free(iname);
        return -1;
	}
    free(iname);
    return 0;
}

int flexiblas_mgmt_hook_option_set_string(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc,
        char *cfg_name, char *optname, char* optval) {
    size_t len;
	char *iname = NULL;
    csc_ini_error_t ret;
    csc_ini_file_t *ini;

    if (config==NULL) {
        return -1;
    }

    if (check_opt_name(optname)) {
        DPRINTF_ERROR(0, "Option name \"%s\" is reserved for internal use.\n", optname);
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

    len = 6 + strlen(cfg_name);
    iname = malloc(sizeof(char *) * len);
    snprintf(iname, len, "HOOK-%s", cfg_name);
	iname = csc_struppercase(iname);

    ret = csc_ini_setstring(ini, iname, optname, optval);
	if ( ret != CSC_INI_SUCCESS ) {
		printf("Failed to set the %s option entry for %s. Exit.\n", optname, cfg_name);
        free(iname);
        return -1;
	}
    free(iname);
    return 0;
}

int flexiblas_mgmt_hook_option_set_float(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc,
        char *cfg_name, char *optname, double optval) {
    size_t len;
	char *iname = NULL;
    csc_ini_error_t ret;
    csc_ini_file_t *ini;

    if (config==NULL) {
        return -1;
    }

    if (check_opt_name(optname)) {
        DPRINTF_ERROR(0, "Option name \"%s\" is reserved for internal use.\n", optname);
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

    len = 6 + strlen(cfg_name);
    iname = malloc(sizeof(char *) * len);
    snprintf(iname, len, "HOOK-%s", cfg_name);
	iname = csc_struppercase(iname);

    ret = csc_ini_setfloat(ini, iname, optname, optval);
	if ( ret != CSC_INI_SUCCESS ) {
		printf("Failed to set the %s option entry for %s. Exit.\n", optname, cfg_name);
        free(iname);
        return -1;
	}
    free(iname);
    return 0;
}


int flexiblas_mgmt_hook_get_active_internal(flexiblas_mgmt_t * config, flexiblas_mgmt_location_t loc, int *nelem, char ***list )
{
    csc_ini_error_t ret;
    csc_ini_file_t *ini;
    char *tmp;
    char *saveptr;
    char *token;
    char **outptr;
    char *parse;
    int n;
    int found = 0;

    if (config==NULL) {
        return -1;
    }
    ini = loc_to_ini(loc, config);
    if (!ini) return -1;

    ret = csc_ini_getstring(ini, CSC_INI_DEFAULT_SECTION, "hooks_enabled", &tmp);
    if ( ret == CSC_INI_SUCCESS ) {
        found = 1;
    }

    if (found) {
        n = 0;
        parse = strdup(tmp);
        token = strtok_r(parse, ":,", &saveptr);
        outptr = NULL;
        while( token!= NULL) {
            n++;
            outptr = (char **) realloc(outptr, sizeof(char *) * n);
            outptr[n-1] = strdup(token);
            token = strtok_r(NULL, ":,", &saveptr);
        }
        free(parse);
        *nelem = n;
        *list = outptr;

        return 0;
    } else {
        *nelem = 0;
        *list = 0;
        return 0;
    }
}


int flexiblas_mgmt_hook_enable(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc, char *hook)
{
    char *newstring;
    char **hooks;
    int nhooks;
    int i ;
    int already = 0;
    size_t len = 0;
    csc_ini_file_t *ini;
    char *hooku;

    if (config==NULL) {
        return -1;
    }

    ini = loc_to_ini(loc, config);
    if (!ini) return -1;

    hooku = __struppercase(strdup(hook));
    if ( !hooku ) return -1;

    if (! __flexiblas_hook_exists(hooku)) {
        DPRINTF_ERROR(0, "Hook %s does not exits.\n", hook);
        return -1;
    }

    flexiblas_mgmt_hook_get_active_internal(config, loc, &nhooks, &hooks);

    already = 0;
    for (i = 0; i < nhooks; i++) {
        if ( csc_strcasecmp(hooku, hooks[i]) == 0) {
            already = 1;
        }
        len += strlen(hooks[i]) + 4;

    }
    if (already) {
        for (i = 0; i < nhooks; i ++) {
            free(hooks[i]);
        }
        free(hooks);
        free(hooku);
        return 0;
    }

    len += strlen(hooku) + 4;
    newstring = malloc(len*sizeof(char));
    newstring[0] = 0;

    for (i = 0; i < nhooks; i++) {
        if ( i !=0 ) {
            strcat(newstring, ",");
        }
        strcat(newstring, hooks[i]);
    }
    if ( nhooks > 0 ){
        strcat(newstring, ",");
    }
    strcat(newstring, hooku);

    csc_ini_setstring(ini, CSC_INI_DEFAULT_SECTION, "hooks_enabled", newstring);

    free(newstring);
    for (i = 0; i < nhooks; i ++) {
        free(hooks[i]);
    }
    free(hooks);
    free(hooku);
    return 0;

}

int flexiblas_mgmt_hook_disable(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc, const char *hook)
{
    char *newstring;
    char **hooks;
    int nhooks;
    int i ;
    size_t len = 0;
    csc_ini_file_t *ini;

    if (config==NULL) {
        return -1;
    }

    ini = loc_to_ini(loc, config);
    if ( !ini ) return -1;

    flexiblas_mgmt_hook_get_active_internal(config, loc, &nhooks, &hooks);
    if ( nhooks == 0) {
        return 0;
    }

    for (i = 0; i < nhooks; i++) {
        len += strlen(hooks[i]) + 4;
    }
    if ( len == 0 ) len = 1;
    newstring = malloc(len*sizeof(char));
    newstring[0] = 0;

    if ( nhooks == 1 && strcasecmp(hooks[0], hook) == 0) {
        strcat(newstring, "");
    } else  {
        int k = 0;
        for (i = 0; i < nhooks; i++) {
            if ( strcasecmp(hooks[i], hook) != 0 ) {
                if ( k != 0 ) {
                    strcat(newstring, ",");
                }
                strcat(newstring, hooks[i]);
                k++;
             }
        }
    }

    csc_ini_setstring(ini, CSC_INI_DEFAULT_SECTION, "hooks_enabled", newstring);

    free(newstring);
    for (i = 0; i < nhooks; i ++) {
        free(hooks[i]);
    }
    free(hooks);
    return 0;

}

int flexiblas_mgmt_hook_disable_all(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc)
{
    csc_ini_file_t *ini;

    if (config==NULL) {
        return -1;
    }
    ini = loc_to_ini(loc, config);
    if ( !ini) return -1;

    csc_ini_key_remove(ini, CSC_INI_DEFAULT_SECTION, "hooks_enabled");

    return 0;
}




/* Hook management  */
int flexiblas_mgmt_hook_get_active(flexiblas_mgmt_t * config, flexiblas_mgmt_location_t *loc, int *nelem, char ***list ) {
    int i ;
    csc_ini_error_t ret;
    csc_ini_file_t *ini;
    char *tmp;
    char *saveptr;
    char *token;
    char **outptr;
    char *parse;
    int n;
    int found = 0;

    if (config==NULL) {
        return -1;
    }

    for (i = FLEXIBLAS_MGMT_LOCATION_COUNT-1; i >= 0; i--) {
        ini = idx_to_ini(i,config);

        ret = csc_ini_getstring(ini, CSC_INI_DEFAULT_SECTION, "hooks_enabled", &tmp);
        if ( ret == CSC_INI_SUCCESS ) {
            found = 1;
            break;
        }

    }



    if (found) {
        *loc = idx_to_loc(i);
        n = 0;
        parse = strdup(tmp);
        token = strtok_r(parse, ":,", &saveptr);
        outptr = NULL;
        while( token!= NULL) {
            if (__flexiblas_hook_exists(token)){
                n++;
                outptr = (char **) realloc(outptr, sizeof(char *) * n);
                outptr[n-1] = strdup(token);
            } else {
                DPRINTF_WARN(0, "Enabled hook %s was not found. Skipped.\n", token);
            }
            token = strtok_r(NULL, ":,", &saveptr);
        }
        free(parse);
        *nelem = n;
        *list = outptr;

        return 0;
    } else {
        *loc = -1;
        *nelem = 0;
        *list = 0;
        return 0;
    }

}

