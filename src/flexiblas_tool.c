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
 * Copyright (C) Martin Koehler, 2013-2015
 */




#include "flexiblas.h"
#ifdef __WIN32__
#define strtok_r strtok_s
#endif 

#include <unistd.h>
#include <sys/types.h>
#include <errno.h>
#include <stdlib.h>
#include <limits.h>
#ifdef __linux__ 
#define _GNU_SOURCE 
#endif 

#include <getopt.h>

#define USER_CONFIG 1 
#define GLOBAL_CONFIG 2 

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
	char *def_str; 
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

static int is_valid_bool ( const char * str ) {
	if ( str == NULL ) return 0; 
	if ( str[0]=='0' || str[0] == '1') return 1; 
	return 0; 
}

char **  __flexiblas_additional_paths = NULL;
int __flexiblas_count_additional_paths = 0; 

properties_t properties[] =  {
	{ "verbose", "0", PROP_INT }, 
	{ "profile", "0", PROP_BOOL },
	{ "profile_file", "stdout", PROP_STRING},
	{ NULL, NULL,  PROP_INT} 
}; 


static void init_default_search_path() {
	char searchpath[] = FLEXIBLAS_DEFAULT_LIB_PATH;
	char *path;
	char *r; 

	path = strtok_r(searchpath,":", &r);
	while ( path != NULL ) {
		__flexiblas_add_path(path);	
		path = strtok_r(NULL, ":",&r);
	}
}

static char *uppercase(char *str) {
	char *ret = str; 
	if ( str == NULL ) return NULL; 
	while (*str != '\0') {
		*str = toupper(*str); 
		str++; 		
	}
	return ret; 
}




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
	printf("  <options> add NAME sharedlibrary.so <\"comment string\"> <ILP64>\n");
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


static void list_blas(csc_ini_file_t *config) {
	csc_ini_iterator_t sec_iterator = NULL; 
	csc_ini_section_t  *sec; 
	char *sec_name; 
	char *tmp = NULL;
	int ilp64 = 0;

	while ((sec = csc_ini_section_iterator(config, &sec_iterator)) != NULL ) {
		if ( (sec_name = csc_ini_getsectionname(sec)) == CSC_INI_DEFAULT_SECTION) continue; 
		printf(" %s\n", sec_name);
		if ( (csc_ini_section_getstring(sec, "library", &tmp) != CSC_INI_SUCCESS) ) {
			printf("   not usable, library not set.\n");
		} else {
			printf("   library = %s\n", tmp);
		}
		if ( csc_ini_section_getstring(sec, "comment", &tmp) == CSC_INI_SUCCESS ) {
			printf("   comment = %s\n", tmp);
		}
#if 0
        /* Reenable in Version 1.4.   */
		if ( csc_ini_section_getinteger(sec, "ilp64", &ilp64) == CSC_INI_SUCCESS) {
			printf("   ilp64   = %d ( %d byte integers )\n", ilp64, (int)(ilp64?sizeof(int64_t):sizeof(int32_t)));
		} else {
			printf("   ilp64   = -  Information provided by the backend if not: assuming 4 byte integers.\n");
		}
#endif 
		printf("\n"); 
	}
	return; 
}

static void list_path(csc_ini_file_t *config) {
	csc_ini_section_t *sec; 
	csc_ini_iterator_t iter = NULL; 
	csc_ini_kvstore_t  *kv; 

	sec = csc_ini_getsection(config, CSC_INI_DEFAULT_SECTION); 

	while ( (kv = csc_ini_kvstore_iterator(sec, &iter)) != NULL ) {
		char *key = csc_ini_getkey(kv); 
		if ( key[0] == 'p' && key[1]=='a' && key[2]=='t' && key[3] == 'h'){
			printf("   %s\n", csc_ini_getvalue(kv)); 
		}
	}
}

static void print_config() {
	int i; 
	char * system_config_file, *user_config_file; 
	csc_ini_file_t config_system; 
	csc_ini_file_t config_user; 
	char *default_system; 
	char *default_user; 
	int pos = 0; 

	__flexiblas_print_copyright(0); 
	printf("\n");
	csc_ini_empty(&config_user); 
	csc_ini_empty(&config_system); 

	init_default_search_path(); 

	/* Load Configs  */
	system_config_file  = __flexiblas_getenv(FLEXIBLAS_ENV_GLOBAL_RC);
	user_config_file = __flexiblas_getenv(FLEXIBLAS_ENV_USER_RC);

	if ( csc_ini_load(system_config_file, &config_system, CSC_INI_LOAD_SECTION_UPPERCASE) != CSC_INI_SUCCESS) {
		printf("Global config  %s does not exists.\n", system_config_file);
		printf("\n");
	}
	if ( csc_ini_load(user_config_file, &config_user, CSC_INI_LOAD_SECTION_UPPERCASE) != CSC_INI_SUCCESS) {
		printf("User config  %s does not exist\n", user_config_file);
		printf("\n");
	}

	printf("System-wide BLAS backends:\n");
	list_blas(&config_system); 
	printf("\n");
	
	printf("User defiled BLAS backends:\n");
	list_blas(&config_user); 
	printf("\n");

	printf("Additional library paths:\n");
	for ( i = 0; i < __flexiblas_count_additional_paths; i++){
		printf("   %s\n", __flexiblas_additional_paths[i]);
	}
	list_path(&config_system); 
	list_path(&config_user); 

	printf("\n");
	csc_ini_getstring(&config_user, CSC_INI_DEFAULT_SECTION, "default", &default_user); 
	csc_ini_getstring(&config_system, CSC_INI_DEFAULT_SECTION, "default", &default_system); 

	printf("Default BLAS:\n");
	if ( default_system == NULL && default_user == NULL ) {
		printf("   No default BLAS set. Using NETLIB as fallback.\n");
	} else if ( default_user == NULL ) {
		printf("   System wide BLAS default: %s\n", default_system);
	} else if ( default_system == NULL ) {
		printf("   User BLAS default: %s\n", default_user);
	} else {
		printf("   System wide BLAS default: %s\n", default_system);
		printf("   ... replaced by User BLAS default: %s\n", default_user);
	}
	printf("\n");
	printf("Runtime properties:\n");
	pos = 0; 
	while ( properties[pos].name != NULL ) {
		csc_ini_getstring(&config_system, CSC_INI_DEFAULT_SECTION, properties[pos].name, &default_system); 
		csc_ini_getstring(&config_user,   CSC_INI_DEFAULT_SECTION, properties[pos].name, &default_user); 

		if ( default_user != NULL ) {
			printf("   %s = %s\n", properties[pos].name, default_user);
		} else if ( default_system != NULL ) {
			printf("   %s = %s\n", properties[pos].name, default_system);
		} else {
			printf("   %s = %s\n", properties[pos].name, properties[pos].def_str);
		}
		pos ++; 
	}
	

	csc_ini_free(&config_system); 
	csc_ini_free(&config_user); 
	free(user_config_file); 
	free(system_config_file); 
	__flexiblas_free_paths();

	return; 
}

static void set_blas(int mode, char* name) {
	char *config_file, *second_file; 
	csc_ini_file_t config, second_config; 
	csc_ini_error_t ret; 
	char *iname = NULL; 


	if (mode == USER_CONFIG) {
		config_file  = __flexiblas_getenv(FLEXIBLAS_ENV_USER_RC);
		second_file  = __flexiblas_getenv(FLEXIBLAS_ENV_GLOBAL_RC);
	} else {
		config_file  = __flexiblas_getenv(FLEXIBLAS_ENV_GLOBAL_RC);
	}
	csc_ini_empty(&config); 
	csc_ini_empty(&second_config); 

	ret =  csc_ini_load(config_file, &config, CSC_INI_LOAD_SECTION_UPPERCASE); 
	if ( ret != CSC_INI_SUCCESS && ret != CSC_INI_FILEOPEN){
		printf("Failed to load %s. Exit\n", config_file);
		goto failed; 
	}
	/* Ignore if readable or not   */
	if (mode == USER_CONFIG) {
		ret = csc_ini_load(second_file, &second_config, CSC_INI_LOAD_SECTION_UPPERCASE); 
		if ( ret != CSC_INI_SUCCESS && ret != CSC_INI_FILEOPEN) {
			printf("Failed to load %s. Exit\n", config_file);
			goto failed; 
		}
	}

	if (name == NULL ) {
		ret = csc_ini_key_remove(&config, CSC_INI_DEFAULT_SECTION, "default"); 
		if ( ret != CSC_INI_NOSECTION && ret != CSC_INI_NOKEY && ret != CSC_INI_SUCCESS){
			printf("Failed to delete default BLAS from %s. Exit\n", config_file);
			goto failed; 
		}
	} else {
		iname = strdup(name); 
		iname = uppercase(iname);

		if ( csc_ini_getsection(&config, iname) == NULL && csc_ini_getsection(&second_config, iname) == NULL ) {
			printf("Choosen BLAS backend not found. Exit\n");
			if (iname != NULL) free(iname); 
			goto failed;
		}

		if ( csc_ini_setstring(&config, CSC_INI_DEFAULT_SECTION, "default", iname) != CSC_INI_SUCCESS) {
			printf("Failed to set default to %s. Exit\n", iname);
			if (iname != NULL) free(iname); 
			goto failed; 
		}
	}
	ret = csc_ini_write(config_file, &config); 

	if ( ret == CSC_INI_FILEOPEN ) {
		printf("Cannot open %s for writing. Exit\n", config_file);
		goto failed; 
	}

	if ( ret != CSC_INI_SUCCESS) {
		printf("Undefined error during storing the default in %s. Exit\n", config_file);
		goto failed; 
	}
	
	csc_ini_free(&config); 
	csc_ini_free(&second_config); 
	free(config_file); 
	return; 
failed: 
	free(config_file); 
	csc_ini_free(&second_config); 
	csc_ini_free(&config); 
	exit(-1); 
	return;
}

static void add_blas (int mode, const char *name, const char *blas, const char *comment, int ilp64) {
	char *config_file; 
	csc_ini_file_t config; 
	csc_ini_error_t ret;
	csc_ini_section_t *sec = NULL; 
	char *iname = NULL; 

	if ( name == NULL || blas == NULL) {
		printf("Internal failure. \n");
		exit(-1); 
	}

	if (mode == USER_CONFIG) {
		config_file  = __flexiblas_getenv(FLEXIBLAS_ENV_USER_RC);
	} else {
		config_file  = __flexiblas_getenv(FLEXIBLAS_ENV_GLOBAL_RC);
	}
	csc_ini_empty(&config); 

	ret =  csc_ini_load(config_file, &config, CSC_INI_LOAD_SECTION_UPPERCASE); 
	if ( ret != CSC_INI_SUCCESS && ret != CSC_INI_FILEOPEN){
		printf("Failed to load %s. Exit\n", config_file);
		goto failed; 
	}

	iname = strdup(name); 
	iname = uppercase(iname); 

	sec = csc_ini_getsection(&config, iname); 
	if ( sec != NULL ) {
		printf("BLAS backend %s exists in %s. Please remove it before adding a new one.\n", iname, config_file);
		free(iname);
		csc_ini_free(&config);
		exit(0);
	}

	ret = csc_ini_setstring(&config, iname, "library", blas); 
	if ( ret != CSC_INI_SUCCESS ) {
		printf("Failed to set the library entry for %s. Exit.\n", iname);
		goto failed; 
	}
	ret = csc_ini_setinteger(&config, iname, "ilp64", ilp64);
	if ( ret != CSC_INI_SUCCESS) {
		printf("Failed to set the ilp64 entry for %s. Exit.\n", iname);
		goto failed; 
	}
	if ( comment != NULL) {
		ret = csc_ini_setstring(&config, iname, "comment", comment); 
		if ( ret != CSC_INI_SUCCESS) {
			printf("Failed to set the comment for %s. Exit.\n", iname);
			goto failed; 
		}
	}

	ret = csc_ini_write(config_file, &config); 

	if ( ret == CSC_INI_FILEOPEN ) {
		printf("Cannot open %s for writing. Exit\n", config_file);
		goto failed; 
	}

	if ( ret != CSC_INI_SUCCESS) {
		printf("Undefined error during storing the default in %s. Exit\n", config_file);
		goto failed; 
	}



	free(iname); 
	csc_ini_free(&config); 
	return; 

failed:
	free(iname); 
	csc_ini_free(&config); 
	exit(-1); 
	return; 
}

static void remove_blas (int mode, const char *name) {
	char *config_file; 
	csc_ini_file_t config; 
	csc_ini_error_t ret;
	char *iname = NULL; 
	char *def_blas = NULL;

	if ( name == NULL ) {
		printf("Internal failure. \n");
		exit(-1); 
	}

	if (mode == USER_CONFIG) {
		config_file  = __flexiblas_getenv(FLEXIBLAS_ENV_USER_RC);
	} else {
		config_file  = __flexiblas_getenv(FLEXIBLAS_ENV_GLOBAL_RC);
	}
	csc_ini_empty(&config); 

	ret =  csc_ini_load(config_file, &config, CSC_INI_LOAD_SECTION_UPPERCASE); 
	if ( ret != CSC_INI_SUCCESS && ret != CSC_INI_FILEOPEN){
		printf("Failed to load %s. Exit\n", config_file);
		goto failed; 
	}

	
	iname = strdup(name); 
	iname = uppercase(iname); 

	if (csc_ini_getstring(&config, CSC_INI_DEFAULT_SECTION, "default", &def_blas ) != CSC_INI_SUCCESS) {
		def_blas = NULL;
	}

	ret = csc_ini_section_remove(&config, iname); 
	if ( ret != CSC_INI_SUCCESS ) {
		printf("Failed to remove the  entry for %s. Exit.\n", iname);
		goto failed; 
	}

	if ( def_blas!= NULL &&  strcasecmp(def_blas, iname) == 0 ){
		printf("Removed BLAS which is set to be the default. Resetting BLAS to \"NETLIB\"\n");
		csc_ini_setstring(&config, CSC_INI_DEFAULT_SECTION, "default", "NETLIB");		
	}
	
	ret = csc_ini_write(config_file, &config); 

	if ( ret == CSC_INI_FILEOPEN ) {
		printf("Cannot open %s for writing. Exit\n", config_file);
		goto failed; 
	}

	if ( ret != CSC_INI_SUCCESS) {
		printf("Undefined error during storing the default in %s. Exit\n", config_file);
		goto failed; 
	}



	free(iname); 
	csc_ini_free(&config); 
	return; 

failed:
	free(iname); 
	csc_ini_free(&config); 
	exit(-1); 
	return; 
}


static void  set_property(int mode, const char *name, char *value) 
{
	char *config_file; 
	csc_ini_file_t config;
	csc_ini_error_t ret; 
	size_t pos = 0; 
	int found = 0 ; 
	long ivalue = 0; 


	if (mode == USER_CONFIG) {
		config_file  = __flexiblas_getenv(FLEXIBLAS_ENV_USER_RC);
	} else {
		config_file  = __flexiblas_getenv(FLEXIBLAS_ENV_GLOBAL_RC);
	}
	csc_ini_empty(&config); 


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
		goto failed;  
	}

        /* Reset the default  */
	if ( value == NULL ) value = properties[pos].def_str; 
	
	if ( properties[pos].type == PROP_INT ) {
		errno = 0; 
		if (!is_valid_int(value)) {
			printf("The property must be an integer. Exit\n");
			goto failed; 
		}
	   	ivalue = atoi(value); 
	}

	if (properties[pos].type == PROP_BOOL ) {
		if (!is_valid_bool(value)){
			printf("The property must be a boolean value. Exit\n");
			goto failed; 
		}
	}

	ret =  csc_ini_load(config_file, &config, CSC_INI_LOAD_SECTION_UPPERCASE); 
	if ( ret != CSC_INI_SUCCESS && ret != CSC_INI_FILEOPEN){
		printf("Failed to load %s. Exit\n", config_file);
		goto failed; 
	}

	if (properties[pos].type == PROP_INT ) {
		ret = csc_ini_setinteger(&config, CSC_INI_DEFAULT_SECTION, name, ivalue); 
	} else {
		ret = csc_ini_setstring(&config, CSC_INI_DEFAULT_SECTION, name, value); 
	}

	if ( ret != CSC_INI_SUCCESS) {
		printf("Setting property %s failed. Exit.\n", name);
		goto failed; 
	}


	/* Write Output  */
	ret = csc_ini_write(config_file, &config); 

	if ( ret == CSC_INI_FILEOPEN ) {
		printf("Cannot open %s for writing. Exit\n", config_file);
		goto failed; 
	}

	if ( ret != CSC_INI_SUCCESS) {
		printf("Undefined error during storing the default in %s. Exit\n", config_file);
		goto failed; 
	}
	
	csc_ini_free(&config); 
	free(config_file); 
	return; 
failed: 
	free(config_file); 
	csc_ini_free(&config); 
	exit(-1); 
	return;

}

int main(int argc, char **argv)
{
	int config_mode; 
	int choice;
	int add_opt =0; 
	int remaim_opt = 0; 
	int todo = 0; 



	/* Determine Config mode  */
	if ( getuid() == 0 ) {
		config_mode = GLOBAL_CONFIG; 
	} else {
		config_mode = USER_CONFIG; 
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
			{0,0,0,0}
		};

		int option_index = 0;

		/* Argument parameters:
			no_argument: " "
			required_argument: ":"
			optional_argument: "::" */

		choice = getopt_long( argc, argv, "vhgu", long_options, &option_index);

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
				config_mode = GLOBAL_CONFIG; 
				break; 
			case 'u':
				config_mode = USER_CONFIG; 
				break; 			
			default:
				/* Not sure how to get here... */
				return EXIT_FAILURE;
		}
	}

	/* Deal with non-option arguments here */
	if (optind >= argc ) {
		printf("Missing argument.\n");
		print_usage(argv[0]); 
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
		print_usage(argv[0]); 
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
				if ( config_mode == USER_CONFIG) {
					printf("Removing user default BLAS setting.\n");
				} else {
					printf("Removing global default BLAS setting.\n");
				}
				set_blas(config_mode, NULL); 
			} else {
				if ( config_mode == USER_CONFIG) {
					printf("Setting user default BLAS to %s.\n", argv[add_opt]);
				} else {
					printf("Setting global default BLAS to %s.\n", argv[add_opt]);
				}
				set_blas(config_mode, argv[add_opt]); 
				return 0; 
			}
			break; 
		case TODO_ADD:
			{
				char *name = NULL; 
				char *blas = NULL; 
				char *comment = NULL; 
				int ilp64=0;

				if ( remaim_opt < 2 || remaim_opt > 4) {
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
					if ( strcasecmp(argv[add_opt+2], "ilp64") == 0 ){
						ilp64 = 1;
						comment = NULL;
					} else {
						ilp64 = 0;
						comment = argv[add_opt+2]; 
					}
				} else {
					name = argv[add_opt]; 
					blas = argv[add_opt+1]; 
					comment = argv[add_opt+2]; 
                	if ( strcasecmp(argv[add_opt+3], "ilp64") == 0 ){
                        ilp64 = 1;
                    } else {
                        ilp64 = 0;
                    }
				}
				add_blas(config_mode, name, blas, comment, ilp64); 
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
				remove_blas(config_mode, blas); 
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
				set_property(config_mode, argv[add_opt], argv[add_opt+1]); 
			else 
				set_property(config_mode, argv[add_opt], NULL); 

			break; 
		default:
			 printf("Invalid Command.s\n");
			 print_usage(argv[0]); 
			 return -1; 			
	}


	return 0; 
}

