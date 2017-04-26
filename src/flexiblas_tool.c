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
 * Copyright (C) Martin Koehler, 2013-2016
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <errno.h>
#include <stdlib.h>
#include <limits.h>
#ifdef __linux__ 
#define _GNU_SOURCE 
#endif 

#include <getopt.h>



#include "flexiblas.h"

#define TODO_HELP 1 
#define TODO_LIST 2 
#define TODO_DEFAULT 3 
#define TODO_ADD 4
#define TODO_REMOVE 5 
#define TODO_PROP 6 

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

static int is_valid_int(const char *str)
{
	if ( str == NULL ) 
		return  0; 

	if (*str == '-')
		++str;
	if (!*str)
		return 0;
	while (*str)
	{
		if (!isdigit(*str))
			return 0;
		else
			++str;
	}

	return 1;
}

char **  __flexiblas_additional_paths = NULL;
int __flexiblas_count_additional_paths = 0; 

properties_t properties[] =  {
	{ "verbose", FLEXIBLAS_PROP_VERBOSE, PROP_INT }, 
	{ "profile", FLEXIBLAS_PROP_PROFILE, PROP_BOOL },
	{ "profile_file", FLEXIBLAS_PROP_PROFILE_FILE, PROP_STRING},
	{ NULL, FLEXIBLAS_PROP_VERBOSE,  PROP_INT} 
}; 



static void print_usage(const char *prgmname) {
    char * 	system_config_file  = __flexiblas_getenv(FLEXIBLAS_ENV_GLOBAL_RC);
	char * user_config_file = __flexiblas_getenv(FLEXIBLAS_ENV_USER_RC);
	printf("The flexiblas tool helps to set the user default BLAS backend for\n");
	printf("FlexiBLAS. The tool modifies the FlexiBLAS configuration filesand sets the\n");
	printf("appropritate default entry in it.\n");
	printf("\n");
	printf("Usage: \n");
	printf(" %s \n", prgmname); 
	printf("  help                        Print this information and exit.\n");
	printf("  list                        List all configured BLAS libraries.\n");
	printf("  <options> default BLASNAME  Sets the default BLAS backend in ~/.flexiblasrc\n");
	printf("                              or in the system wide config\n");
	printf("  <options> default           Removes the default setting from ~/.flexiblasrc\n");
	printf("                              or frome system wide config\n");
	printf("  <options> add NAME sharedlibrary.so <\"comment string\"> \n");
	printf("                              Add a new BLAS backend called \"NAME\"\n");
    printf("                              to the configuration. \n");
	printf("  <options> remove NAME       Removes a BLAS backed called \"NAME\" \n");
    printf("                              from the configuration. \n");
	printf("  <options> set property value \n");
	printf("                              Set a property in the configuration file.\n"); 
    printf("                              If the value is not given the property is\n"); 
    printf("                              reset to its default.\n");
	printf("\n");
	printf("The following values can be added for <options>:\n"); 
	printf(" -u, --user      Edit the user's flexiblasrc configuration\n"); 
    printf("                 (default, except of root).\n");
	printf(" -g, --global    Edit the global flexiblasrc configuration\n"); 
    printf("                 default in case of root).\n");
    printf(" -H, --host      Edit the host's flexiblasrc configuration\n");
	printf(" -h, --help      Print this information and exit.\n");
	printf(" -v, --version   Print the versionformation and exit.\n");
	printf("\n");
	printf("Possible properties are: \n");
	printf(" verbose         Sets the verbosity of FlexiBLAS (integer, 0 = quiet) \n");
	printf(" profile         Enables/Disables the profiling  (boolean, 0 or 1) \n");
	printf(" profile_file    Sets the output file if profiling is enabled. (default=stdout)\n");
    printf("\n");
    printf("user configuration file:   \n\t%s\n", user_config_file);
    printf("global configuration file: \n\t%s\n", system_config_file);
	printf("\n");
    free(user_config_file); 
    free(system_config_file); 
	return; 
}


static void list_blas(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc) {
    void *iter_helper;
    char blas_name[FLEXIBLAS_MGMT_MAX_BUFFER_LEN];
    char library_name[FLEXIBLAS_MGMT_MAX_BUFFER_LEN];
    char comment[FLEXIBLAS_MGMT_MAX_BUFFER_LEN];

    if ( config == NULL) return;
    
    iter_helper = NULL;
    while ( flexiblas_mgmt_list_blas(config, loc,  blas_name, library_name, comment, &iter_helper) > 0)
    {
		printf(" %s\n", blas_name);
        if (strlen(library_name) == 0) {
    			printf("   not usable, library not set.\n");
        }  else {
			printf("   library = %s\n", library_name);
		}
		printf("   comment = %s\n", comment);
    }
	
	return; 
}

static void list_path(flexiblas_mgmt_t *config, flexiblas_mgmt_location_t loc) {
    char path[FLEXIBLAS_MGMT_MAX_BUFFER_LEN];
    void * iter_helper = NULL;
    memset(path, 0, FLEXIBLAS_MGMT_MAX_BUFFER_LEN);

    if ( config == NULL) return;
    
    iter_helper = NULL;
    while ( flexiblas_mgmt_list_paths(config, loc, path, &iter_helper) > 0)
    {
	    if ( strlen(path) > 0 ) 		printf("   %s\n", path); 
    }
}

static void print_config() {
	char * system_config_file, *user_config_file, *host_config_file;
    flexiblas_mgmt_t *config;
	char default_system[FLEXIBLAS_MGMT_MAX_BUFFER_LEN], default_user[FLEXIBLAS_MGMT_MAX_BUFFER_LEN];
    char default_host[FLEXIBLAS_MGMT_MAX_BUFFER_LEN];
    char paths[FLEXIBLAS_MGMT_MAX_BUFFER_LEN];
    void *path_tmp;
    flexiblas_mgmt_location_t loc;
	
    __flexiblas_print_copyright(0); 
	printf("\n");
	

	/* Load Configs  */
	system_config_file  = flexiblas_mgmt_location(FLEXIBLAS_GLOBAL);
	user_config_file = flexiblas_mgmt_location(FLEXIBLAS_USER);
    host_config_file = flexiblas_mgmt_location(FLEXIBLAS_HOST);
    config = flexiblas_mgmt_load_config();


    /*-----------------------------------------------------------------------------
     *  List BLAS
     *-----------------------------------------------------------------------------*/

	printf("System-wide BLAS backends (%s):\n", system_config_file);
	list_blas(config, FLEXIBLAS_GLOBAL); 
	printf("\n");
	
	printf("User defined BLAS backends (%s):\n", user_config_file);
	list_blas(config, FLEXIBLAS_USER); 
	printf("\n");

    printf("User defined BLAS backends per Host (%s):\n", host_config_file);
	list_blas(config, FLEXIBLAS_HOST); 
	printf("\n");

	printf("Additional library paths:\n");
    path_tmp = NULL;
    while (flexiblas_mgmt_list_default_paths(paths, &path_tmp) > 0){
		printf("   %s\n", paths);
	}
	list_path(config, FLEXIBLAS_GLOBAL); 
	list_path(config, FLEXIBLAS_USER); 
    list_path(config, FLEXIBLAS_HOST);

	printf("\n");

    flexiblas_mgmt_get_default(config, FLEXIBLAS_GLOBAL, default_system);
    flexiblas_mgmt_get_default(config, FLEXIBLAS_USER, default_user);
    flexiblas_mgmt_get_default(config, FLEXIBLAS_HOST, default_host);

	printf("Default BLAS:\n");
    printf("    System: %s\n", default_system);
    printf("    User:   %s\n", default_user);
    printf("    Host:   %s\n", default_host);

    flexiblas_mgmt_get_active_default(config, &loc, default_system);
    printf("    Active Default: %s (%s)\n", default_system, flexiblas_mgmt_location_to_string(loc));

    /* Properties  */
	printf("Runtime properties:\n");
    int prop_verbose, prop_profile;
    char profile_file[FLEXIBLAS_MGMT_MAX_BUFFER_LEN];

    flexiblas_mgmt_get_active_property(config, &loc, FLEXIBLAS_PROP_VERBOSE, &prop_verbose);
    printf("   verbose = %d (%s) \n", prop_verbose, flexiblas_mgmt_location_to_string(loc));
    flexiblas_mgmt_get_active_property(config, &loc, FLEXIBLAS_PROP_PROFILE, &prop_profile);
    printf("   profile = %d (%s) \n", prop_profile, flexiblas_mgmt_location_to_string(loc));
    flexiblas_mgmt_get_active_property(config, &loc, FLEXIBLAS_PROP_PROFILE_FILE, &profile_file);
    printf("   profile_file = %d (%s) \n", prop_profile, flexiblas_mgmt_location_to_string(loc));

    flexiblas_mgmt_free_config(config);
	free(user_config_file); 
	free(system_config_file);
    free(host_config_file);

	return; 
}

static void set_blas(flexiblas_mgmt_location_t loc, char* name) {
    flexiblas_mgmt_t *config;
    int     ret = 0;

    config = flexiblas_mgmt_load_config();
    if (config == NULL) {
        printf("Failed to open configuration files. Abort.\n");
        return;
    }

    if ( flexiblas_mgmt_set_default(config, loc, name)) {
        printf("Failed to set default BLAS in %s to %s.\n", flexiblas_mgmt_location_to_string(loc), name);
        flexiblas_mgmt_free_config(config);
        return;
    }
    ret = flexiblas_mgmt_write_config2(config,loc);
    flexiblas_mgmt_free_config(config);
    if ( ret ) {
        printf("Failed to write the configuration to %s.\n", flexiblas_mgmt_location_to_string(loc));
        exit(-1);
    }
    return;

}

static void add_blas (flexiblas_mgmt_location_t loc, char *name, char *blas, char *comment) {
    flexiblas_mgmt_t *config;
    int     ret = 0;

    config = flexiblas_mgmt_load_config();
    if (config == NULL) {
        printf("Failed to open configuration files. Abort.\n");
        return;
    }

    if ( flexiblas_mgmt_blas_add(config, loc, name, blas, comment) ) {
        flexiblas_mgmt_free_config(config);
        printf("Failed to add BLAS (%s , %s).\n", name, blas);
        return;
    }

    ret = flexiblas_mgmt_write_config2(config,loc);
    flexiblas_mgmt_free_config(config);
    if ( ret ) {
        printf("Failed to write the configuration to %s.\n", flexiblas_mgmt_location_to_string(loc));
        exit(-1);
    }
    return;
}

static void remove_blas (flexiblas_mgmt_location_t loc, char *name) {
    flexiblas_mgmt_t *config;
    int     ret = 0;

    config = flexiblas_mgmt_load_config();
    if (config == NULL) {
        printf("Failed to open configuration files. Abort.\n");
        return;
    }

    if ( flexiblas_mgmt_blas_remove(config, loc, name) ) {
        flexiblas_mgmt_free_config(config);
        printf("Failed to remove BLAS %s from %s.\n", name, flexiblas_mgmt_location_to_string(loc));
        return;
    }

    ret = flexiblas_mgmt_write_config(config);
    flexiblas_mgmt_free_config(config);
    if ( ret ) {
        printf("Failed to write the configuration to %s.\n", flexiblas_mgmt_location_to_string(loc));
        exit(-1);
    }
    return;
}


static void  set_property(flexiblas_mgmt_location_t loc, const char *name, char *value) 
{
    flexiblas_mgmt_t *config;
	size_t pos = 0; 
	int found = 0 ;
    int ivalue = 0;
    int ret = 0 ;

    config = flexiblas_mgmt_load_config();
    if (config == NULL) {
        printf("Failed to open configuration files. Abort.\n");
        return;
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
        exit(-1);
	}

    if ( value == NULL ){
        flexiblas_mgmt_set_property(config, loc, properties[pos].e_prop, NULL);
        ret = flexiblas_mgmt_write_config2(config,loc);
        flexiblas_mgmt_free_config(config);

        if ( ret ) {
            printf("Failed to write the configuration to %s.\n", flexiblas_mgmt_location_to_string(loc));
            exit(-1);
         }

        return;
    }

	if ( properties[pos].type == PROP_INT ) {
		errno = 0; 
		if (!is_valid_int(value)) {
			printf("The property must be an integer. Exit\n");
            flexiblas_mgmt_free_config(config);
            exit(-1);
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
            exit(-1);
         }

       return;

}

int main(int argc, char **argv)
{
    flexiblas_mgmt_location_t config_location;
	int choice;
	int add_opt =0; 
	int remaim_opt = 0; 
	int todo = 0; 



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
			{0,0,0,0}
		};

		int option_index = 0;

		/* Argument parameters:
			no_argument: " "
			required_argument: ":"
			optional_argument: "::" */

		choice = getopt_long( argc, argv, "vhguH", long_options, &option_index);

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
	}


	remaim_opt--; 
	add_opt++; 

	if ( todo == 0 ) {
		printf("Invalid or missing argument.\n");
        printf("Please run '%s help' to see a list of possible arguments.\n", argv[0]);
		return -1; 
	}

	/* Select the work  */
	switch (todo) {
		case TODO_HELP:
			print_usage(argv[0]); 
			return 0; 
		case TODO_LIST:
			print_config(); 
			return 0;
		case TODO_DEFAULT: 
			if ( remaim_opt == 0) {
				if ( config_location == FLEXIBLAS_USER) {
					printf("Removing user default BLAS setting.\n");
                } else if ( config_location == FLEXIBLAS_HOST) {
					printf("Removing host default BLAS setting.\n");
                } else if ( config_location == FLEXIBLAS_GLOBAL ){
					printf("Removing global default BLAS setting.\n");
				} else {
                    printf("Unknown configuration location.\n");
                    exit(-1);
				}
				set_blas(config_location, NULL); 
			} else {
				if ( config_location == FLEXIBLAS_USER) {
					printf("Setting user default BLAS to %s.\n", argv[add_opt]);
                } else if ( config_location == FLEXIBLAS_HOST) {
					printf("Setting host default BLAS to %s.\n", argv[add_opt]);
                } else if ( config_location == FLEXIBLAS_GLOBAL) {
					printf("Setting system default BLAS to %s.\n", argv[add_opt]);
				} else {
                    printf("Unknown configuration location.\n");
                    exit(-1);
				}
				set_blas(config_location, argv[add_opt]); 
				return 0; 
			}
			break; 
		case TODO_ADD:
			{
				char *name = NULL; 
				char *blas = NULL; 
				char *comment = NULL; 

				if ( remaim_opt < 2 || remaim_opt > 3) {
					printf("Missing Arguments or too much arguments.\n");
					print_usage(argv[0]);
					return -1; 
				}
				if ( remaim_opt == 2 ) {
					name = argv[add_opt]; 
					blas = argv[add_opt+1]; 
				} else if (remaim_opt == 3) {
					name = argv[add_opt]; 
					blas = argv[add_opt+1]; 
					comment = argv[add_opt+2]; 
				} 
				add_blas(config_location, name, blas, comment); 
				return 0; 
			}
		case TODO_REMOVE: 
			{
				char * blas; 
				if ( remaim_opt != 1 ) {
					printf("Missing Arguments or too much arguments.\n");
					print_usage(argv[0]);
					return -1; 
				}
				blas = argv[add_opt]; 
				remove_blas(config_location, blas); 
				return 0; 
			}
			break; 
		case TODO_PROP: 
			if (remaim_opt < 1 || remaim_opt > 2  ) {
				printf("Missing Arguments or too much arguments.\n");
				print_usage(argv[0]);
				return -1; 
			}
			if (remaim_opt == 2) 
				set_property(config_location, argv[add_opt], argv[add_opt+1]); 
			else 
				set_property(config_location, argv[add_opt], NULL); 

			break; 
		default:
			 printf("Invalid Command.s\n");
			 /* print_usage(argv[0]);  */
			 return -1; 			
	}


	return 0; 
}

