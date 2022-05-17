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
 * Copyright (C) Martin Koehler, 2013-2022
 */



#include <stdio.h>
#include <stdlib.h>
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

#include <getopt.h>



#include "flexiblas.h"
#include "flexiblas_config.h"
#include "paths.h"
#include "helper.h"
#include "hooks.h"
#include "cscutils/strutils.h"
#include "tool/tool.h"

#define TODO_HOOK 100
#define TODO_HOOK_SET 101
#define TODO_HOOK_UNSET 102
#define TODO_HELP 1
#define TODO_LIST 2
#define TODO_DEFAULT 3
#define TODO_ADD 4
#define TODO_REMOVE 5
#define TODO_PROP 6
#define TODO_PRINT 7
#define TODO_LISTHOOKS 8
#define TODO_SHOWHOOK 9
#define TODO_ENABLEHOOK 10
#define TODO_DISABLEHOOK 11
#define TODO_DISABLEALL 12
#define TODO_LISTENABLED 13
#define TODO_LISTACTIVE 14

#ifdef INTEGER8
    #define     ENV_FLEXIBLAS_LIBRARY_PATH "FLEXIBLAS64_LIBRARY_PATH"
#else
    #define     ENV_FLEXIBLAS_LIBRARY_PATH "FLEXIBLAS_LIBRARY_PATH"
#endif


int pipe_output = 0;

typedef enum {
	PROP_INT,
	PROP_BOOL,
	PROP_STRING
} prop_type_t;

typedef struct _properties_t {
	char *name;
    flexiblas_mgmt_property_t e_prop;
	prop_type_t type;
} properties_t;

typedef int (*list_func_t) (flexiblas_mgmt_t *, flexiblas_mgmt_location_t, char *, char *, char *, void **);



// char **  __flexiblas_additional_paths = NULL;
// int __flexiblas_count_additional_paths = 0;

properties_t properties[] =  {
	{ "verbose", FLEXIBLAS_PROP_VERBOSE, PROP_INT },
    { "nolapack", FLEXIBLAS_PROP_NOLAPACK, PROP_INT},
	{ NULL, FLEXIBLAS_PROP_VERBOSE,  PROP_INT}
};



static void print_usage(const char *prgmname) {
    char * 	system_config_file  = __flexiblas_getenv(FLEXIBLAS_ENV_GLOBAL_RC);
	char * user_config_file = __flexiblas_getenv(FLEXIBLAS_ENV_USER_RC);
    char * host_config_file = __flexiblas_getenv(FLEXIBLAS_ENV_HOST_RC);
    char * env_config_file = __flexiblas_getenv(FLEXIBLAS_ENV_ENV_RC);
    char * system_config_dir = __flexiblas_getenv(FLEXIBLAS_ENV_GLOBAL_RC_DIR);

	printf("The flexiblas tool helps to set the user default BLAS backend for\n");
	printf("FlexiBLAS. The tool modifies the FlexiBLAS configuration files and sets the\n");
	printf("appropriate default entry in it.\n");
	printf("\n");
	printf("Usage: \n");
	printf(" %s <options> command\n", prgmname);
    printf("\n");
    printf("Commands:\n");
	printf("  help              Print this information and exit.\n");
    printf("  print             Print the whole configuration.\n");
	printf("  list              List all configured BLAS libraries.\n");
	printf("  default BLASNAME  Sets the default BLAS backend in ~/.%s\n", FLEXIBLAS_RC);
	printf("                    or the select configuration file.\n");
	printf("  default           Removes the default setting from ~/.%s\n", FLEXIBLAS_RC);
	printf("                    or from the selected configuration file.\n");
	printf("  add NAME sharedlibrary.so <\"comment string\"> \n");
	printf("                    Add a new BLAS backend called \"NAME\"\n");
    printf("                    to the selected configuration. \n");
	printf("  remove NAME       Removes a BLAS backed called \"NAME\" \n");
    printf("                    from the selected configuration. \n");
    printf("  set property value \n");
	printf("                    Set a property in the configuration file.\n");
    printf("                    If the value is not given the property is\n");
    printf("                    reset to its default.\n");
    printf("\n");
    printf("Hook management commands (only for user and host config):\n");
    printf("  hook list                 List all available hooks.\n");
    printf("  hook show hook-name       Show information about the hook. This includes\n");
    printf("                            all configuration options.\n");
    printf("  hook enabled              List enabled hooks from all configurations.\n");
    printf("  hook active               List currently active hooks.\n");
    printf("  hook enable hook-name     Enable a hook.\n");
    printf("  hook disable hook-name    Disable a hook.\n");
    printf("  hook disableall           Disable all hooks and removes the hook_enabled\n");
    printf("                            field from config.\n");
    printf("  hook set hook-name option value\n");
    printf("                            Set an option for a specific hook.\n");
    printf("  hook remove hook-name option\n");
    printf("                            Remove an option setting for a specific hook.\n");
   	printf("\n");
	printf("The following values can be added for <options>:\n");
	printf(" -u, --user      Edit the user's flexiblasrc configuration\n");
    printf("                 (default, except of root).\n");
	printf(" -g, --global    Edit the global flexiblasrc configuration\n");
    printf("                 default in case of root).\n");
    printf(" -E, --environment\n");
    printf("                 Edit the configuration file given by the\n");
    printf("                 FLEXIBLAS_CONFIG environment variable.\n");
    printf(" -H, --host      Edit the host's flexiblasrc configuration\n");
    printf(" -p, --pipe      Pipe/Script compatible output.\n");
	printf(" -h, --help      Print this information and exit.\n");
	printf(" -v, --version   Print the version information and exit.\n");
	printf("\n");
	printf("Possible properties are: \n");
	printf(" verbose         Sets the verbosity of FlexiBLAS (integer, 0 = quiet) \n");
    printf(" nolapack        Do not load the LAPACK function from the backend.\n");
    printf("\n");
    printf("global configuration file:        %s\n", system_config_file);
	printf("global configuration directory:   %s\n", system_config_dir);
    printf("user configuration file:          %s\n", user_config_file);
    printf("host configuration file:          %s\n", host_config_file);
	if ( env_config_file ) printf("environment configuration file:   %s\n", env_config_file);

    printf("\n");
    if (user_config_file) free(user_config_file);
    if (system_config_file) free(system_config_file);
    if (host_config_file) free(host_config_file);
    if (env_config_file) free(env_config_file);
    if (system_config_dir) free(system_config_dir);
	return;
}


int print_config() {
	char * system_config_file, *user_config_file, *host_config_file, *env_config_file, *system_config_dir; ;
    flexiblas_mgmt_t *config;
	char default_system[FLEXIBLAS_MGMT_MAX_BUFFER_LEN], default_user[FLEXIBLAS_MGMT_MAX_BUFFER_LEN];
    char default_host[FLEXIBLAS_MGMT_MAX_BUFFER_LEN], default_env[FLEXIBLAS_MGMT_MAX_BUFFER_LEN];
    flexiblas_mgmt_location_t loc;
    int i;

    /* Disable Pipe Printing */
    pipe_output = 0;

    __flexiblas_print_copyright(0);
	printf("\n");


	/* Load Configurations  */
	system_config_file  = flexiblas_mgmt_location(FLEXIBLAS_GLOBAL);
	user_config_file = flexiblas_mgmt_location(FLEXIBLAS_USER);
    host_config_file = flexiblas_mgmt_location(FLEXIBLAS_HOST);
    env_config_file  = flexiblas_mgmt_location(FLEXIBLAS_ENV);
    system_config_dir = flexiblas_mgmt_location(FLEXIBLAS_GLOBAL_DIR);

    config = flexiblas_mgmt_load_config();

    /*-----------------------------------------------------------------------------
     *  List BLAS
     *-----------------------------------------------------------------------------*/
    printf("Configured BLAS libraries:\n");
    printf("System-wide (%s):\n", system_config_file);
    print_blas(config, FLEXIBLAS_GLOBAL, NULL);
    printf("\nSystem-wide from config directory (%s)\n", system_config_dir);
    print_blas(config, FLEXIBLAS_GLOBAL_DIR, NULL);
    printf("\nUser config (%s):\n", user_config_file);
    print_blas(config, FLEXIBLAS_USER, NULL);
    printf("\nHost config (%s):\n", host_config_file);
    print_blas(config, FLEXIBLAS_HOST, NULL);
    if(env_config_file){
        printf("\nEnviroment (%s):\n", env_config_file);
        print_blas(config, FLEXIBLAS_ENV,NULL);
    }

	printf("\n");

    /*-----------------------------------------------------------------------------
     *  List Hooks
     *-----------------------------------------------------------------------------*/
    list_all_hooks();

	printf("\n");


	printf("Backend and hook search paths:\n");
    for (i = 0; i < __flexiblas_count_additional_paths; i++) {
        printf("  %s\n", __flexiblas_additional_paths[i]);
    }


  	printf("\n");

    flexiblas_mgmt_get_default(config, FLEXIBLAS_GLOBAL, default_system);
    flexiblas_mgmt_get_default(config, FLEXIBLAS_USER, default_user);
    flexiblas_mgmt_get_default(config, FLEXIBLAS_HOST, default_host);
    if (env_config_file) flexiblas_mgmt_get_default(config, FLEXIBLAS_ENV, default_env);

	printf("Default BLAS:\n");
    printf("    System:       %s\n", default_system);
    printf("    User:         %s\n", default_user);
    printf("    Host:         %s\n", default_host);
    if (env_config_file)  printf("    Environment:   %s\n", default_env);

    flexiblas_mgmt_get_active_default(config, &loc, default_system);
    printf("    Active Default: %s (%s)\n", default_system, flexiblas_mgmt_location_to_string(loc));

    /* Properties  */
	printf("Run-time properties:\n");
    int prop_verbose;
    flexiblas_mgmt_get_active_property(config, &loc, FLEXIBLAS_PROP_VERBOSE, &prop_verbose);
    printf("   verbose = %d (%s) \n", prop_verbose, flexiblas_mgmt_location_to_string(loc));

    flexiblas_mgmt_free_config(config);

    if ( system_config_file ) free(system_config_file);
    if ( user_config_file) free(user_config_file);
    if ( host_config_file) free(host_config_file);
    if ( env_config_file) free(env_config_file);
    if ( system_config_dir) free(system_config_dir);
	return 0;
}




static int  set_property(flexiblas_mgmt_location_t loc, const char *name, char *value)
{
    flexiblas_mgmt_t *config;
    size_t pos = 0;
    int found = 0 ;
    int ivalue = 0;
    int ret = 0 ;

    config = flexiblas_mgmt_load_config();
    if (config == NULL) {
        printf("Failed to open configuration files. Abort.\n");
        return -1;
    }
    /* Check if the Option is valid    */
    while (properties[pos].name != NULL ) {
        if ( strcmp(name, properties[pos].name) == 0) {
            found = 1;
            break;
        }
        pos ++;
    }
    if ( found == 0 ) {
        printf("Unknown property %s. Exit\n", name);
        flexiblas_mgmt_free_config(config);
        return -1;
    }

    if ( value == NULL ){
        flexiblas_mgmt_set_property(config, loc, properties[pos].e_prop, NULL);
        ret = flexiblas_mgmt_write_config2(config,loc);
        flexiblas_mgmt_free_config(config);

        if ( ret ) {
            printf("Failed to write the configuration to %s.\n", flexiblas_mgmt_location_to_string(loc));
            return -1;
        }

        return 0;
    }

    if ( properties[pos].type == PROP_INT ) {
        errno = 0;
        if (!csc_str_is_valid_int(value)) {
            printf("The property must be an integer. Exit\n");
            flexiblas_mgmt_free_config(config);
            return -1;
        }
        ivalue = atoi(value);
        flexiblas_mgmt_set_property(config, loc, properties[pos].e_prop, &ivalue);
    } else {
        flexiblas_mgmt_set_property(config, loc, properties[pos].e_prop, value);
    }

    ret = flexiblas_mgmt_write_config2(config,loc);
    flexiblas_mgmt_free_config(config);

    if ( ret ) {
        printf("Failed to write the configuration to %s.\n", flexiblas_mgmt_location_to_string(loc));
        return -1;
    }

    return 0;

}

int main(int argc, char **argv)
{
    flexiblas_mgmt_location_t config_location;
	int choice;
	int add_opt =0;
	int remaim_opt = 0;
	int todo = 0;
    int ecode = 0;



	/* Determine Config mode  */
	if ( getuid() == 0 ) {
        config_location = FLEXIBLAS_GLOBAL;
	} else {
        config_location = FLEXIBLAS_USER;
	}

	while (1)
	{
		static struct option long_options[] =
		{
			/* Use flags like so:
			{"verbose",	no_argument,	&verbose_flag, 'V'}*/
			/* Argument styles: no_argument, required_argument, optional_argument */
			{"version", no_argument,	0,	'v'},
			{"help",	no_argument,	0,	'h'},
			{"user", no_argument, 0, 'u'},
			{"global", no_argument, 0, 'g'},
            {"host", no_argument, 0, 'H'},
            {"environment", no_argument, 0, 'E'},
            {"pipe", no_argument, 0, 'p'},
			{0,0,0,0}
		};

		int option_index = 0;

		/* Argument parameters:
			no_argument: " "
			required_argument: ":"
			optional_argument: "::" */

		choice = getopt_long( argc, argv, "vhguHEp", long_options, &option_index);

		if (choice == -1)
			break;

		switch( choice )
		{
			case 'v':
				printf("FlexiBLAS version %s\n", FLEXIBLAS_VERSION);
				return 0;
			case 'h':
				__flexiblas_print_copyright(0);
			        print_usage(argv[0]);
				return 0;
			case 'g':
                config_location = FLEXIBLAS_GLOBAL;
				break;
			case 'u':
                config_location = FLEXIBLAS_USER;
				break;
            case 'H':
                config_location = FLEXIBLAS_HOST;
                break;
            case 'E':
                config_location = FLEXIBLAS_ENV;
                break;
            case 'p':
                pipe_output = 1;
                break;
			default:
				/* Not sure how to get here... */
				return EXIT_FAILURE;
		}
	}

	/* Deal with non-option arguments here */
	if (optind >= argc ) {
		printf("Missing argument.\n");
        printf("Please run '%s help' to see a list of possible arguments.\n", argv[0]);
		return -1;
	}

	/* Parse the remaining options  */
	remaim_opt = argc - optind;
	add_opt = optind ;

	if ( strcmp(argv[add_opt], "help") == 0) {
		todo = TODO_HELP;
	} else if (strcmp(argv[add_opt], "list") == 0) {
		todo = TODO_LIST;
	} else if (strcmp(argv[add_opt], "default") == 0 ) {
		todo = TODO_DEFAULT;
	} else if ( strcmp(argv[add_opt], "add" ) == 0 ) {
		todo = TODO_ADD;
	} else if (strcmp(argv[add_opt], "remove") == 0 ) {
		todo = TODO_REMOVE;
	} else if (strcmp(argv[add_opt], "set") == 0 ) {
		todo = TODO_PROP;
    } else if (strcmp(argv[add_opt], "hook") == 0 ) {
        todo = TODO_HOOK;
    } else if (strcmp(argv[add_opt], "print") == 0 ) {
        todo = TODO_PRINT;
    }

    /* Todos for hooks  */
    if ( todo == TODO_HOOK && config_location != FLEXIBLAS_GLOBAL) {
        add_opt++;
        remaim_opt--;
        if ( remaim_opt <= 0) {
            printf("additional command missing.\n");
            print_usage(argv[0]);
            return -1;
        }
        if (strcmp(argv[add_opt], "list") == 0 ) {
            todo = TODO_LISTHOOKS;
        } else if (strcmp(argv[add_opt], "show") == 0 ) {
            todo = TODO_SHOWHOOK;
        } else if (strcmp(argv[add_opt], "enable") == 0 ) {
            todo = TODO_ENABLEHOOK;
        } else if (strcmp(argv[add_opt], "disable") == 0 ) {
            todo = TODO_DISABLEHOOK;
        } else if (strcmp(argv[add_opt], "disableall") == 0 ) {
            todo = TODO_DISABLEALL;
        } else if (strcmp(argv[add_opt], "set") == 0 ) {
            todo = TODO_HOOK_SET;
        } else if (strcmp(argv[add_opt], "unset") == 0 ) {
            todo = TODO_HOOK_UNSET;
        } else if ( strcmp(argv[add_opt], "enabled") == 0 ) {
            todo = TODO_LISTENABLED;
        } else if ( strcmp(argv[add_opt] , "active") == 0 ) {
            todo = TODO_LISTACTIVE;
        }
    } else if ( todo == TODO_HOOK && config_location == FLEXIBLAS_GLOBAL) {
        printf("The hook management is not possible in the global config file.\n");
        printf("Please use the user or the host configuration file for handling\n");
        printf("hooks.\n\n\n");
        print_usage(argv[0]);
        return -1;
    }

	remaim_opt--;
	add_opt++;

	if ( todo == 0 ) {
		printf("Invalid or missing argument.\n");
        printf("Please run '%s help' to see a list of possible arguments.\n", argv[0]);
		return -1;
	}

    flexiblas_mgmt_init();
	/* Select the work  */
	switch (todo) {
        /* Print the help  */
		case TODO_HELP:
			print_usage(argv[0]);
            ecode = 0;
            break;

        /* Print the whole configuration  */
        case TODO_PRINT:
            ecode = print_config();
            break;

        /* List all BLAS libraries.  */
		case TODO_LIST:
            ecode = list_all_blas();
			break;

        /* Set the default.  */
		case TODO_DEFAULT:
			if ( remaim_opt == 0) {
                if ( config_location == FLEXIBLAS_ENV) {
                	printf("Removing environment default BLAS setting.\n");
                } else if ( config_location == FLEXIBLAS_USER) {
					printf("Removing user default BLAS setting.\n");
                } else if ( config_location == FLEXIBLAS_HOST) {
					printf("Removing host default BLAS setting.\n");
                } else if ( config_location == FLEXIBLAS_GLOBAL ){
					printf("Removing global default BLAS setting.\n");
				} else {
                    printf("Unknown configuration location.\n");
                    ecode = 1;
                    break;
				}
				ecode = set_blas(config_location, NULL);
			} else {
                if ( config_location == FLEXIBLAS_ENV) {
					printf("Setting environment default BLAS to %s.\n", argv[add_opt]);
                } else if ( config_location == FLEXIBLAS_USER) {
					printf("Setting user default BLAS to %s.\n", argv[add_opt]);
                } else if ( config_location == FLEXIBLAS_HOST) {
					printf("Setting host default BLAS to %s.\n", argv[add_opt]);
                } else if ( config_location == FLEXIBLAS_GLOBAL) {
					printf("Setting system default BLAS to %s.\n", argv[add_opt]);
				} else {
                    printf("Unknown configuration location.\n");
                    ecode = 1;
				}
				ecode = set_blas(config_location, argv[add_opt]);
			}
			break;

        /* Add a BLAS library   */
		case TODO_ADD:
			{
				char *name = NULL;
				char *blas = NULL;
				char *comment = NULL;

				if ( remaim_opt < 2 || remaim_opt > 3) {
					printf("Missing Arguments or too much arguments.\n");
					print_usage(argv[0]);
					ecode = -1;
                    break;
				}
				if ( remaim_opt == 2 ) {
					name = argv[add_opt];
					blas = argv[add_opt+1];
				} else if (remaim_opt == 3) {
					name = argv[add_opt];
					blas = argv[add_opt+1];
					comment = argv[add_opt+2];
				}
				ecode = add_blas(config_location, name, blas, comment);
				break;
			}

        /* Remove a BLAS library from config.  */
		case TODO_REMOVE:
			{
				char * blas;
				if ( remaim_opt != 1 ) {
					printf("Missing Arguments or too much arguments.\n");
					print_usage(argv[0]);
                    ecode = 1;
                    break;
				}
				blas = argv[add_opt];
				ecode = remove_blas(config_location, blas);
			}
			break;

        /* Set properties  */
		case TODO_PROP:
			if (remaim_opt < 1 || remaim_opt > 2  ) {
				printf("Missing Arguments or too much arguments.\n");
				print_usage(argv[0]);
				ecode = 1;
                break;
			}
			if (remaim_opt == 2)
				ecode = set_property(config_location, argv[add_opt], argv[add_opt+1]);
			else
				ecode = set_property(config_location, argv[add_opt], NULL);

			break;

        /* List all available hooks.  */
        case TODO_LISTHOOKS:
            ecode = list_all_hooks();
            break;

        /* Show details about a hook.  */
        case TODO_SHOWHOOK:
            if ( remaim_opt != 1) {
                printf("Only one argument required.\n");
                print_usage(argv[0]);
                ecode = 1;
                break;
            }

            ecode = show_hook(argv[add_opt]);
            break;

        /* Enable a hook in a config  */
        case TODO_ENABLEHOOK:
            if ( remaim_opt != 1) {
                printf("Hook name needs to be given.\n");
                print_usage(argv[0]);
                ecode = 1;
                break;
            }
            ecode = enable_hook(config_location, argv[add_opt]);
            break;

         /* Disable a single hook in a config  */
         case TODO_DISABLEHOOK:
            if ( remaim_opt != 1) {
                printf("Hook name needs to be given.\n");
                print_usage(argv[0]);
                ecode = 1;
                break;
            }
            ecode = disable_hook(config_location, argv[add_opt]);
            break;

         /* Disable all hooks in a config   */
         case TODO_DISABLEALL:
            if (remaim_opt != 0 ) {
                printf("No arguments required.\n");
                print_usage(argv[0]);
                ecode = 1;
                break;
            }
            ecode = disable_all_hooks(config_location);
            break;
        case TODO_HOOK_SET:
            {
                if ( remaim_opt != 3 ) {
                    ecode = -1;
                    printf("hook set needs three arguments.\n");
                    break;
                }
                char * hookname = argv[add_opt];
                char * option   = argv[add_opt+1];
                char * value    = argv[add_opt+2];

                ecode = hook_option_set(config_location, hookname, option, value);
            }
            break;
        case TODO_HOOK_UNSET:
            {
                if ( remaim_opt != 2 ) {
                    ecode = -1;
                    printf("hook set needs two arguments.\n");
                    break;
                }
                char * hookname = argv[add_opt];
                char * option   = argv[add_opt+1];

                ecode = hook_option_unset(config_location, hookname, option);
            }
            break;
        case TODO_LISTENABLED:
            {
                ecode = list_enabled_hooks();
            }
            break;
        case TODO_LISTACTIVE:
            {
                ecode = list_active_hooks();
            }
            break;
        default:
			 printf("Invalid Command.\n");
			 /* print_usage(argv[0]);  */
             ecode = 1;
	}

    flexiblas_mgmt_exit();


	return ecode;
}

