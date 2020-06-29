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
 * Copyright (C) Martin Koehler, 2013-2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdlib.h>
#include <limits.h>
#include <dlfcn.h>
#ifdef __linux__
#define _GNU_SOURCE
#endif

#include "flexiblas.h"
#include "flexiblas_config.h"
#include "paths.h"
#include "helper.h"
#include "hooks.h"
#include "cscutils/strutils.h"
#include "tool.h"

int disable_all_hooks(flexiblas_mgmt_location_t loc)
{
    flexiblas_mgmt_t *config;
    int     ret = 0;

    config = flexiblas_mgmt_load_config();
    if (config == NULL) {
        printf("Failed to open configuration files. Abort.\n");
        return 1;
    }

    flexiblas_mgmt_hook_disable_all(config, loc);

    ret = flexiblas_mgmt_write_config2(config, loc);
    flexiblas_mgmt_free_config(config);
    if ( ret ) {
        printf("Failed to write the configuration to %s.\n", flexiblas_mgmt_location_to_string(loc));
        return 1;
    }
    return 0 ;

}

int disable_hook(flexiblas_mgmt_location_t loc, const char *name)
{
    flexiblas_mgmt_t *config;
    int ret = 0;

    config = flexiblas_mgmt_load_config();
    if (config == NULL) {
        printf("Failed to open configuration files. Abort.\n");
        return 1;
    }
    if ( flexiblas_mgmt_hook_disable(config, loc, name)) {
        printf("Disabling hook %s failed.\n", name);
    } else {
        printf("Disable hook %s successful.\n", name);
        ret = flexiblas_mgmt_write_config2(config,loc);
        if ( ret ) {
            flexiblas_mgmt_free_config(config);
            printf("Failed to write the configuration to %s.\n", flexiblas_mgmt_location_to_string(loc));
            return 1;
        }

    }
    flexiblas_mgmt_free_config(config);
    return 0 ;
}

int enable_hook(flexiblas_mgmt_location_t loc, char *name)
{
    flexiblas_mgmt_t *config;
    int ret = 0;
    int ecode = 0;

    if ( loc == FLEXIBLAS_GLOBAL) {
        fprintf(stderr, "Enabling global hooks is not allowed.\n");
        exit(-1);
    }

    config = flexiblas_mgmt_load_config();
    if (config == NULL) {
        printf("Failed to open configuration files. Abort.\n");
        return 1;
    }

    if (!  __flexiblas_hook_exists(name)) {
         printf("Hook %s does not exists.\n", name);
         ecode = 1; goto fin;
    }

    if ( flexiblas_mgmt_hook_enable(config, loc, name)) {
        printf("Enabling hook %s failed.\n", name);
    } else {
        printf("Enable hook %s successful.\n", name);
        ret = flexiblas_mgmt_write_config2(config,loc);
        if ( ret ) {
            printf("Failed to write the configuration to %s.\n", flexiblas_mgmt_location_to_string(loc));
            ecode = 1; goto fin;
        }
    }

fin:
    flexiblas_mgmt_free_config(config);
    return ecode;
}

int  show_hook(char *name)
{
    char *sofile;
    int ecode = 0;
    void *handle = NULL;

    if (! __flexiblas_hook_exists(name)) {
         printf("No hook named %s is found.\n", name);
         ecode = 1;
         goto fin;
    }

    sofile = __flexiblas_hook_sofile(name);
    if (!sofile) {
         printf("No hook named %s is found.\n", name);
         ecode = 1;
         goto fin;
    }

    dlerror();
    handle = __flexiblas_dlopen(sofile, RTLD_LAZY | RTLD_LOCAL, NULL);
    if ( !handle) {
        printf("Cannot open %s as shared library. (error = %s)\n", sofile, dlerror());
        ecode = 1;
        goto fin;
    }

    flexiblas_hook_register_t *reg = (flexiblas_hook_register_t *) dlsym(handle, "flexiblas_register");

    printf("Name:          %s\n", reg->name);
    printf("Configuration: %s\n", reg->cfg_name);
    printf("Author(s):     %s\n", reg->authors);
    printf("Description:   %s\n", reg->desc);

    int cnt = 0;
    int nopts = 0;
    flexiblas_option_t *opts = (flexiblas_option_t *) dlsym(handle, "flexiblas_options");
    if ( opts == NULL)
        nopts = 0;
    else {
        cnt = 0;
        while(opts[cnt].name != NULL) cnt++;
        nopts = cnt;
    }

    for (cnt = 0; cnt < nopts; cnt++) {
        printf("\n");
        printf("Option:      %s\n", opts[cnt].name);
        printf("Description: %s\n", opts[cnt].desc);
        printf("Type:        ");
        switch(opts[cnt].type) {
            case FLEXIBLAS_OPTIONS_INT:
                printf("Integer\n"); break;
            case FLEXIBLAS_OPTIONS_STRING:
                printf("String\n"); break;
            case FLEXIBLAS_OPTIONS_FLOAT:
                printf("Float\n");break;
            default:
                ;
        }
        printf("Default:     %s\n", opts[cnt].def);
    }
fin:
    if ( handle != NULL) dlclose(handle);
    return ecode;

}






int list_all_hooks()
{
    char **list1;
    char **list2;
    int nelem;
    int i;

    if ( !pipe_output ) printf("Available hooks:\n");
    __flexiblas_hook_list(&nelem, &list1, &list2);
    for ( i = 0 ; i < nelem; i++ ) {
        if (pipe_output) {
            printf("%s|%s\n", list1[i], list2[i]);
        } else {
            printf("  %s (%s)\n", list1[i], list2[i]);
        }
        free(list1[i]);
        free(list2[i]);
    }
    free(list1);
    free(list2);
    return 0;

}

int list_enabled_hooks()
{
    int i, j;
    flexiblas_mgmt_location_t loc;
    flexiblas_mgmt_t *config;
    int nelem;
    char ** list;

    config = flexiblas_mgmt_load_config();

    if ( ! config ) return -1;

    for ( i = 0; i < 3; i++)
    {
        if ( i == 0 )
            loc = FLEXIBLAS_USER;
        else if ( i == 1 )
            loc = FLEXIBLAS_HOST;
        else if ( i == 2 )
            loc = FLEXIBLAS_ENV;

        if ( flexiblas_mgmt_hook_get_active_internal(config, loc, &nelem, &list))
            continue;

        if (! pipe_output ) {
            printf("Enabled Hooks in %s:\n", flexiblas_mgmt_location_to_string(loc));
        } else {
            if ( loc == FLEXIBLAS_USER)
                printf("user|");
            else if ( loc == FLEXIBLAS_HOST )
                printf("host|");
            else if ( loc == FLEXIBLAS_ENV )
                printf("env|");
        }

        for ( j = 0; j < nelem; j++) {
            if (!pipe_output) {
                printf("- %s\n", list[j]);
            } else {
                printf("%s", list[j]);
                if ( j < nelem-1 )
                    printf("|");
            }
            free(list[j]);
        }

        if (pipe_output) printf("\n");
        free(list);
    }

    if ( config ) flexiblas_mgmt_free_config(config);
    return 0;

}

int list_active_hooks()
{
    int j;
    flexiblas_mgmt_location_t loc;
    flexiblas_mgmt_t *config;
    int nelem;
    char ** list;

    config = flexiblas_mgmt_load_config();

    if ( ! config ) return -1;

    if ( flexiblas_mgmt_hook_get_active(config, &loc, &nelem, &list))
    {
        if ( pipe_output ) printf("none\n");
        else printf("No hooks enabled.\n");
        return 0;
    }

    if (! pipe_output ) {
        printf("Currently active hooks from %s:\n", flexiblas_mgmt_location_to_string(loc));
        if ( nelem == 0 ) {
            printf("Hooks are explicitly disabled in the %s configuration.\n", flexiblas_mgmt_location_to_string(loc));
            printf("You can remove the setting using the \"disableall\" subcommand.\n");
        }
    } else {
        if ( loc == FLEXIBLAS_USER)
            printf("user|");
        else if ( loc == FLEXIBLAS_HOST )
            printf("host|");
        else if ( loc == FLEXIBLAS_ENV )
            printf("env|");
    }

    for ( j = 0; j < nelem; j++) {
        if (!pipe_output) {
            printf("- %s\n", list[j]);
        } else {
            printf("%s", list[j]);
            if ( j < nelem-1 )
                printf("|");
        }
        free(list[j]);
    }

    if (pipe_output) printf("\n");
    free(list);

    if ( config ) flexiblas_mgmt_free_config(config);
    return 0;

}



int hook_option_set(flexiblas_mgmt_location_t loc, char *hookname, char *option, char *value)
{

    char *sofile;
    void *handle;
    flexiblas_hook_register_t *reg = NULL;
    flexiblas_option_t * opts = NULL;
    int nopts, i, found;
    int ecode = 0;
    char *help;
    flexiblas_mgmt_t *config = NULL ;



    if ( ! __flexiblas_hook_exists(hookname)) {
        printf("Hook %s does not exist.\n", hookname);
        return -1;
    }

    sofile = __flexiblas_hook_sofile(hookname);
    if (!sofile) {
         printf("No hook named %s is found.\n", hookname);
         return -1;
    }

    handle = __flexiblas_dlopen(sofile, RTLD_LAZY | RTLD_LOCAL, NULL);
    if ( !handle) {
        printf("Opening hook %s/%s failed.\n", hookname, sofile);
        return -1;
    }
    reg = (flexiblas_hook_register_t *) dlsym(handle, "flexiblas_register");
    opts = (flexiblas_option_t *) dlsym(handle, "flexiblas_options");

    if ( reg == NULL) {
        printf("The shared object %s is not a hook.\n", sofile);
        dlclose(handle);
        return -1;
    }
    if ( opts == NULL) {
        printf("The hook %s does not have any options.\n", reg->cfg_name);
        dlclose(handle);
        return -1;
    }

    nopts = 0;
    while(opts[nopts].name != NULL) nopts++;

    found = -1;
    for (i = 0; i < nopts; i++) {
        if ( strcmp(opts[i].name, option) == 0 ) {
            found = i;
        }
    }
    if (found < 0) {
        printf("Option %s not found in hook %s\n", option, hookname);
        dlclose(handle);
        return -1;
    }

    config = flexiblas_mgmt_load_config();
    if ( !config) {
        printf("Failed to load config.\n");
        ecode = -1;
        goto fin;
    }

    switch ( opts[found].type) {
        case FLEXIBLAS_OPTIONS_INT:
            {
                int v;
                help = NULL;
                v = strtol (value, &help, 10);
                if ( help && !(help[0] == '\0')) {
                    printf("The option is required to be an integer but something different was provided. (value = %s, err = '%s')\n", value, help);
                    ecode = 1;
                    goto fin;
                }
                ecode = flexiblas_mgmt_hook_option_set_int(config, loc, reg->cfg_name, option, v);
            }
            break;
        case FLEXIBLAS_OPTIONS_FLOAT:
            {
                double v;
                v = strtod (value, &help);
                if ( help == value || help[0] != '\0') {
                    printf("The option is required to be a float but something different was provided. (value = %s, err = '%s')\n", value, help);
                    ecode = 1;
                    goto fin;
                }
                ecode = flexiblas_mgmt_hook_option_set_float(config, loc, reg->cfg_name, option, v);
            }
            break;
        case FLEXIBLAS_OPTIONS_STRING:
            ecode = flexiblas_mgmt_hook_option_set_string(config, loc, reg->cfg_name, option, value);
            break;
        default:
            ;

    }
    if (!ecode) {
        ecode = flexiblas_mgmt_write_config2(config, loc);
        if ( ecode ) {
            printf("Failed to write the configuration to %s.\n", flexiblas_mgmt_location_to_string(loc));

        }
    }

fin:
    if ( config ) flexiblas_mgmt_free_config(config);
    dlclose(handle);
    return ecode;
}

int hook_option_unset(flexiblas_mgmt_location_t loc, char *hookname, char *option)
{

    int ecode = 0;
    flexiblas_mgmt_t *config = NULL ;



    if ( ! __flexiblas_hook_exists(hookname)) {
        printf("Hook %s does not exist.\n", hookname);
        return -1;
    }

    config = flexiblas_mgmt_load_config();
    if ( !config) {
        printf("Failed to load config.\n");
        ecode = -1;
        goto fin;
    }

    ecode = flexiblas_mgmt_hook_option_unset(config, loc, hookname, option);
    if (ecode == -2) {
        ecode = 0;
    }

    if (!ecode) {
        ecode = flexiblas_mgmt_write_config2(config, loc);
        if ( ecode ) {
            printf("Failed to write the configuration to %s.\n", flexiblas_mgmt_location_to_string(loc));

        }
    }

fin:
    if ( config ) flexiblas_mgmt_free_config(config);
    return ecode;


}
