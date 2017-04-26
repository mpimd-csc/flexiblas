/* $Id: flexiblas.c 3914 2014-01-08 09:32:15Z komart $ */ 
/* 
 Copyright (C) 2013, 2014  Martin Köhler, koehlerm@mpi-magdeburg.mpg.de

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "flexiblas.h"
#include <errno.h>

#ifndef __WIN32__
#define DLOPEN_FLAGS (RTLD_NOW|RTLD_GLOBAL)
#else 
#define DLOPEN_FLAGS (0) 
#define strtok_r strtok_s 
#include <windows.h> 
#endif 


/*  Initialize global variables. */
void*  __flexiblas_library = NULL; 
int __flexiblas_initialized = 0; 
int __flexiblas_profile = 0;

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


static void flexiblas_load_library (hashtable blas_libary_map, char *blas_default_map ) {
	kv_pair *blas_pair; 
	char *env_FLEXIBLAS=getenv("FLEXIBLAS"); 
	/*-----------------------------------------------------------------------------
	 *  Analyze the FLEXIBLAS environment variable 
	 *-----------------------------------------------------------------------------*/
	if (env_FLEXIBLAS== NULL) {
		blas_pair = flexiblas_hashtable_find(blas_libary_map, blas_default_map);
		if (blas_pair == NULL ) {
			fprintf(stderr, COLOR_RED PRINT_PREFIX "Default BLAS not found.\n" COLOR_RESET);
			abort(); 
		}
		if (__flexiblas_verbose) fprintf(stderr,PRINT_PREFIX "Use default BLAS: %s - %s\n", blas_pair->key, blas_pair->value);
		__flexiblas_library = __flexiblas_dlopen(blas_pair->value, DLOPEN_FLAGS); 
	} else {

		/*-----------------------------------------------------------------------------
		 *  Try to open env_FLEXIBLAS directly and the get the value from the Hashtable 
		 *-----------------------------------------------------------------------------*/
		if (__flexiblas_verbose) fprintf(stderr,PRINT_PREFIX "Trying to use the content of FLEXIBLAS: \"%s\" as shared library.\n", env_FLEXIBLAS);
		__flexiblas_library = __flexiblas_dlopen(env_FLEXIBLAS, DLOPEN_FLAGS );  
		if ( __flexiblas_library == NULL) {
			blas_pair = flexiblas_hashtable_find(blas_libary_map, env_FLEXIBLAS); 
			if (blas_pair == NULL ) {
				fprintf(stderr, COLOR_RED PRINT_PREFIX "BLAS backend  \"%s\" not found. Loading default (%s) instead.\n" COLOR_RESET, env_FLEXIBLAS, blas_default_map);
				blas_pair = flexiblas_hashtable_find(blas_libary_map, blas_default_map); 
				if (blas_pair == NULL ) {
					fprintf(stderr, COLOR_RED PRINT_PREFIX "Default BLAS not found.\n" COLOR_RESET);
					abort(); 
				}				
			}
			if (__flexiblas_verbose) fprintf(stderr,PRINT_PREFIX "Trying to use the flexiblasrc default: \"%s\" - %s\n", blas_pair->key, blas_pair->value);
			__flexiblas_library = __flexiblas_dlopen(blas_pair->value,DLOPEN_FLAGS);  
		}
	}
	/* Load FallBack */
	if ( __flexiblas_library == NULL ) {
		fprintf(stderr, PRINT_PREFIX "No suitable BLAS backend could be loaded. Tring Fallback instead.\n");
		blas_pair = flexiblas_hashtable_find(blas_libary_map, "fallback");
		if ( blas_pair ){
			__flexiblas_library = __flexiblas_dlopen(blas_pair->value,DLOPEN_FLAGS);  
		}
	}
	if ( __flexiblas_library == NULL ) {
		fprintf(stderr, PRINT_PREFIX "Unable to open any BLAS library (choosen: %s). Abort!\n", (env_FLEXIBLAS == NULL)?blas_default_map:env_FLEXIBLAS); 
		abort();
	}

	/*-----------------------------------------------------------------------------
	 *  load info
	 *-----------------------------------------------------------------------------*/
	memset(&__flexiblas_current_blas,0,sizeof(struct flexiblas_info));
#ifdef __WIN32__ 
	flexiblas_info_function h_info = (flexiblas_info_function) GetProcAddress(__flexiblas_library, "__flexiblas_info"); 
#else 
	flexiblas_info_function h_info = dlsym(__flexiblas_library, "__flexiblas_info"); 
#endif 
	if ( h_info ) {
		h_info(&__flexiblas_current_blas); 
		if (__flexiblas_verbose) {
			fprintf(stderr, PRINT_PREFIX "BLAS info:\n"); 
			fprintf(stderr, PRINT_PREFIX " - zdotc_is_intel = %d\n",__flexiblas_current_blas.zdotc_is_intel); 
			fprintf(stderr, PRINT_PREFIX " - zdotu_is_intel = %d\n",__flexiblas_current_blas.zdotu_is_intel); 
			fprintf(stderr, PRINT_PREFIX " - cdotc_is_intel = %d\n",__flexiblas_current_blas.cdotc_is_intel); 
			fprintf(stderr, PRINT_PREFIX " - cdotu_is_intel = %d\n",__flexiblas_current_blas.cdotu_is_intel); 
			fprintf(stderr, PRINT_PREFIX " - scabs1_missing = %d\n",__flexiblas_current_blas.scabs1_missing); 
		}
	}
	/*-----------------------------------------------------------------------------
	 *  load Hooks 
	 *-----------------------------------------------------------------------------*/
	__flexiblas_hook_double(__flexiblas_library); 
	__flexiblas_hook_single(__flexiblas_library); 
	__flexiblas_hook_complex(__flexiblas_library); 
	__flexiblas_hook_complex16(__flexiblas_library); 
	__flexiblas_hook_integer(__flexiblas_library); 
	return; 
}

#ifndef __WIN32__ 
__attribute__((constructor))
#endif 
void flexiblas_init() {
	char *env_FLEXIBLAS_PROFILE=getenv("FLEXIBLAS_PROFILE");
	hashtable blas_libary_map; 
	char * blas_default_map= NULL ; 
	/*-----------------------------------------------------------------------------
	 *  Read Environment Variables 
	 *-----------------------------------------------------------------------------*/
	char *env_FLEXIBLAS_VERBOSE=getenv("FLEXIBLAS_VERBOSE"); 

	if ( __flexiblas_initialized != 0) return; 
	__flexiblas_initialized = 1; 


	if ( env_FLEXIBLAS_VERBOSE != NULL ) {
		__flexiblas_verbose = atoi(env_FLEXIBLAS_VERBOSE); 
	} else {
		__flexiblas_verbose = 0; 
	}
	
	if (env_FLEXIBLAS_PROFILE != NULL) {
		if (atoi(env_FLEXIBLAS_PROFILE) > 0 ) {
			__flexiblas_profile = 1;
		}		
	} else {
		__flexiblas_profile = 0;
	}

	/*-----------------------------------------------------------------------------
	 *  Display Copyright 
	 *-----------------------------------------------------------------------------*/
	if ( __flexiblas_verbose) {
		__flexiblas_print_copyright(1); 
	}


	/*-----------------------------------------------------------------------------
	 *  Read mapping file
	 *  1. /etc/flexiblasrc 
	 *  2. CMAKE_INSTALL_PREFIX/etc/flexiblasrc 
	 *  3. $HOME/.flexiblasrc 
	 *-----------------------------------------------------------------------------*/
	blas_libary_map = flexiblas_hashtable_create(__flexiblas_kv_pair_getkey, __flexiblas_kv_pair_free,31,__flexiblas_kv_hash); 
	if ( blas_libary_map == NULL) {
		fprintf(stderr, PRINT_PREFIX "can not create empty hash table for blas libraries. Abort!\n");
		abort(); 
	}

	__flexiblas_insert_fallback_blas(blas_libary_map);
	{ /* Load System config */
		char * system_config_file  = __flexiblas_getenv(FLEXIBLAS_ENV_GLOBAL_RC);
		__flexiblas_load_config(system_config_file,blas_libary_map, &blas_default_map); 
		free(system_config_file);
	}
	{ /* Load User Config */
		char * user_config_file = __flexiblas_getenv(FLEXIBLAS_ENV_USER_RC);
		__flexiblas_load_config(user_config_file, blas_libary_map, &blas_default_map);
		free(user_config_file);
	}
	init_default_search_path();

	if (!blas_default_map) {
		blas_default_map = strdup("netlib"); 
	}

	/*-----------------------------------------------------------------------------
	 *  Load Library 
	 *-----------------------------------------------------------------------------*/
	flexiblas_load_library(blas_libary_map, blas_default_map); 

	if ( __flexiblas_profile ) {
		if ( atexit ( flexiblas_print_profile ) != 0 ) {
			fprintf(stderr, "Cannot setup Profiling Output \n");
		}
	}
	
	flexiblas_hashtable_freeall(blas_libary_map); 
	if ( blas_libary_map ) free (blas_default_map); 
	__flexiblas_free_paths();
}



/*-----------------------------------------------------------------------------
 *  Cleanup 
 *-----------------------------------------------------------------------------*/
#ifndef __WIN32__ 
__attribute__((destructor)) 
#endif
void flexiblas_exit() {
	if (__flexiblas_verbose ) fprintf(stderr, PRINT_PREFIX "cleanup\n"); 
	if (__flexiblas_library != NULL ) {
#ifdef __WIN32__ 
		FreeLibrary(__flexiblas_library); 
#else
		dlclose(__flexiblas_library); 
#endif
	}

	__flexiblas_library = NULL; 
}

int __flexiblas_loadhook(void *handle, const char *symbol, const char *csymbol, struct flexiblas_blasfn * fn) {
	char cfblas[20]; 
	void *ptr_fsymbol = NULL; 
#ifdef FLEXIBLAS_CBLAS
	char ccblas[20]; 
	void *ptr_csymbol = NULL; 
#endif 

	if ( handle == NULL ) {
		fn ->call_fblas = NULL; 
		fn ->call_cblas = NULL; 
		return 1; 
	}
	snprintf(cfblas, 19, "%s_",symbol); 
	if (__flexiblas_verbose > 1 ) {
		fprintf(stderr, PRINT_PREFIX " Look up: %s", cfblas); 
	}

	// fprintf(stderr, "Look for %s\n", cfblas);
#ifdef __WIN32__
	ptr_fsymbol = GetProcAddress(handle,cfblas);
#else
	ptr_fsymbol = dlsym(handle,cfblas);
#endif 
	if (ptr_fsymbol){
		fn -> call_fblas = ptr_fsymbol; 
	} else {
		// fprintf(stderr, "Look for %s\n",symbol);
#ifdef __WIN32__
		ptr_fsymbol = GetProcAddress(handle,symbol);
#else 
		ptr_fsymbol = dlsym(handle,symbol);
#endif

		if (ptr_fsymbol) {
			fn->call_fblas = ptr_fsymbol; 
		} 
	}
	if ( __flexiblas_verbose > 1) {
		fprintf(stderr, " %s.\n",(fn->call_fblas == NULL)?"failed":"sucess"); 
	}
#ifdef FLEXIBLAS_CBLAS 
	// fprintf(stderr, "Look for %s\n",ccblas);
	if (csymbol!=NULL) {
		snprintf(ccblas, 19, "%s", csymbol); 
	} else {
		snprintf(ccblas, 19, "cblas_%s", symbol); 
	}
#ifdef __WIN32__
	ptr_csymbol = GetProcAddress(handle, ccblas); 
#else 
	ptr_csymbol = dlsym(handle, ccblas); 

#endif
	if (ptr_csymbol) {
		fn -> call_cblas = ptr_csymbol; 
	}
#endif 
	if ( fn->call_fblas == NULL ) 
		return 1; 
	else 
		return 0; 
}

void  flexiblas_print_profile() {
	FILE *output = NULL; 
	char *env_output_file = getenv("FLEXIBLAS_PROFILE_FILE"); 
	if (env_output_file == NULL) {
		output = stderr; 
	} else {
		output = fopen(env_output_file,"w"); 
		if (!output){
			int err = errno; 
			fprintf(stderr, "Opening %s for profile output failed. Use stderr instead. (Reason: %s)\n",env_output_file, strerror(err));
			output = stderr; 
		} 
	}
	fprintf(output, "\n");
	fprintf(output, "*******************************************************************************\n");
	fprintf(output, "* FlexiBLAS Profiling                                                         *\n");
	fprintf(output, "*******************************************************************************\n");
	fprintf(output, "\n");
	fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
	fprintf(output, "* Single Precission BLAS calls.                                               *\n"); 
	fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
	fprintf(output,"Function \t\t Runtime in s\t     Calls \n"); 

	fprintf(output,"%16s \t %11.7e \t %8lu\n","sasum",flexiblas_time_sasum[0],flexiblas_call_sasum[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_sasum",flexiblas_time_sasum[1],flexiblas_call_sasum[1], (flexiblas_sasum.call_cblas == NULL && flexiblas_call_sasum[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","saxpy",flexiblas_time_saxpy[0],flexiblas_call_saxpy[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_saxpy",flexiblas_time_saxpy[1],flexiblas_call_saxpy[1], (flexiblas_saxpy.call_cblas==NULL&&flexiblas_call_saxpy[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","scabs1",flexiblas_time_scabs1[0],flexiblas_call_scabs1[0]);

	fprintf(output,"%16s \t %11.7e \t %8lu\n","scopy",flexiblas_time_scopy[0],flexiblas_call_scopy[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_scopy",flexiblas_time_scopy[1],flexiblas_call_scopy[1], (flexiblas_scopy.call_cblas==NULL&&flexiblas_call_scopy[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","sdot",flexiblas_time_sdot[0],flexiblas_call_sdot[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_sdot",flexiblas_time_sdot[1],flexiblas_call_sdot[1], (flexiblas_sdot.call_cblas==NULL&&flexiblas_call_sdot[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","sdsdot",flexiblas_time_sdsdot[0],flexiblas_call_sdsdot[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_sdsdot",flexiblas_time_sdsdot[1],flexiblas_call_sdsdot[1], (flexiblas_sdsdot.call_cblas==NULL&&flexiblas_call_sdsdot[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","sgbmv",flexiblas_time_sgbmv[0],flexiblas_call_sgbmv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_sgbmv",flexiblas_time_sgbmv[1],flexiblas_call_sgbmv[1], (flexiblas_sgbmv.call_cblas==NULL&&flexiblas_call_sgbmv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","sgemm",flexiblas_time_sgemm[0],flexiblas_call_sgemm[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_sgemm",flexiblas_time_sgemm[1],flexiblas_call_sgemm[1], (flexiblas_sgemm.call_cblas==NULL&&flexiblas_call_sgemm[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","sgemv",flexiblas_time_sgemv[0],flexiblas_call_sgemv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_sgemv",flexiblas_time_sgemv[1],flexiblas_call_sgemv[1], (flexiblas_sgemv.call_cblas==NULL&&flexiblas_call_sgemv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","sger",flexiblas_time_sger[0],flexiblas_call_sger[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_sger",flexiblas_time_sger[1],flexiblas_call_sger[1], (flexiblas_sger.call_cblas==NULL&&flexiblas_call_sger[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","snrm2",flexiblas_time_snrm2[0],flexiblas_call_snrm2[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_snrm2",flexiblas_time_snrm2[1],flexiblas_call_snrm2[1], (flexiblas_snrm2.call_cblas==NULL&&flexiblas_call_snrm2[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","srot",flexiblas_time_srot[0],flexiblas_call_srot[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_srot",flexiblas_time_srot[1],flexiblas_call_srot[1], (flexiblas_srot.call_cblas==NULL&&flexiblas_call_srot[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","srotg",flexiblas_time_srotg[0],flexiblas_call_srotg[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_srotg",flexiblas_time_srotg[1],flexiblas_call_srotg[1], (flexiblas_srotg.call_cblas==NULL&&flexiblas_call_srotg[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","srotm",flexiblas_time_srotm[0],flexiblas_call_srotm[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_srotm",flexiblas_time_srotm[1],flexiblas_call_srotm[1], (flexiblas_srotm.call_cblas==NULL&&flexiblas_call_srotm[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","srotmg",flexiblas_time_srotmg[0],flexiblas_call_srotmg[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_srotmg",flexiblas_time_srotmg[1],flexiblas_call_srotmg[1], (flexiblas_srotmg.call_cblas==NULL&&flexiblas_call_srotmg[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","ssbmv",flexiblas_time_ssbmv[0],flexiblas_call_ssbmv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_ssbmv",flexiblas_time_ssbmv[1],flexiblas_call_ssbmv[1], (flexiblas_ssbmv.call_cblas==NULL&&flexiblas_call_ssbmv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","sscal",flexiblas_time_sscal[0],flexiblas_call_sscal[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_sscal",flexiblas_time_sscal[1],flexiblas_call_sscal[1], (flexiblas_sscal.call_cblas==NULL&&flexiblas_call_sscal[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","sspmv",flexiblas_time_sspmv[0],flexiblas_call_sspmv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_sspmv",flexiblas_time_sspmv[1],flexiblas_call_sspmv[1], (flexiblas_sspmv.call_cblas==NULL&&flexiblas_call_sspmv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","sspr",flexiblas_time_sspr[0],flexiblas_call_sspr[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_sspr",flexiblas_time_sspr[1],flexiblas_call_sspr[1], (flexiblas_sspr.call_cblas==NULL&&flexiblas_call_sspr[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","sspr2",flexiblas_time_sspr2[0],flexiblas_call_sspr2[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_sspr2",flexiblas_time_sspr2[1],flexiblas_call_sspr2[1], (flexiblas_sspr2.call_cblas==NULL&&flexiblas_call_sspr2[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","sswap",flexiblas_time_sswap[0],flexiblas_call_sswap[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_sswap",flexiblas_time_sswap[1],flexiblas_call_sswap[1], (flexiblas_sswap.call_cblas==NULL&&flexiblas_call_sswap[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","ssymm",flexiblas_time_ssymm[0],flexiblas_call_ssymm[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_ssymm",flexiblas_time_ssymm[1],flexiblas_call_ssymm[1], (flexiblas_ssymm.call_cblas==NULL&&flexiblas_call_ssymm[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","ssymv",flexiblas_time_ssymv[0],flexiblas_call_ssymv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_ssymv",flexiblas_time_ssymv[1],flexiblas_call_ssymv[1], (flexiblas_ssymv.call_cblas==NULL&&flexiblas_call_ssymv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","ssyr",flexiblas_time_ssyr[0],flexiblas_call_ssyr[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_ssyr",flexiblas_time_ssyr[1],flexiblas_call_ssyr[1], (flexiblas_ssyr.call_cblas==NULL&&flexiblas_call_ssyr[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","ssyr2",flexiblas_time_ssyr2[0],flexiblas_call_ssyr2[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_ssyr2",flexiblas_time_ssyr2[1],flexiblas_call_ssyr2[1], (flexiblas_ssyr2.call_cblas==NULL&&flexiblas_call_ssyr2[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","ssyr2k",flexiblas_time_ssyr2k[0],flexiblas_call_ssyr2k[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_ssyr2k",flexiblas_time_ssyr2k[1],flexiblas_call_ssyr2k[1], (flexiblas_ssyr2k.call_cblas==NULL&&flexiblas_call_ssyr2k[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","ssyrk",flexiblas_time_ssyrk[0],flexiblas_call_ssyrk[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_ssyrk",flexiblas_time_ssyrk[1],flexiblas_call_ssyrk[1], (flexiblas_ssyrk.call_cblas==NULL&&flexiblas_call_ssyrk[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","stbmv",flexiblas_time_stbmv[0],flexiblas_call_stbmv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_stbmv",flexiblas_time_stbmv[1],flexiblas_call_stbmv[1], (flexiblas_stbmv.call_cblas==NULL&&flexiblas_call_stbmv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","stbsv",flexiblas_time_stbsv[0],flexiblas_call_stbsv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_stbsv",flexiblas_time_stbsv[1],flexiblas_call_stbsv[1], (flexiblas_stbsv.call_cblas==NULL&&flexiblas_call_stbsv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","stpmv",flexiblas_time_stpmv[0],flexiblas_call_stpmv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_stpmv",flexiblas_time_stpmv[1],flexiblas_call_stpmv[1], (flexiblas_stpmv.call_cblas==NULL&&flexiblas_call_stpmv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","stpsv",flexiblas_time_stpsv[0],flexiblas_call_stpsv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_stpsv",flexiblas_time_stpsv[1],flexiblas_call_stpsv[1], (flexiblas_stpsv.call_cblas==NULL&&flexiblas_call_stpsv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","strmm",flexiblas_time_strmm[0],flexiblas_call_strmm[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_strmm",flexiblas_time_strmm[1],flexiblas_call_strmm[1], (flexiblas_strmm.call_cblas==NULL&&flexiblas_call_strmm[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","strmv",flexiblas_time_strmv[0],flexiblas_call_strmv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_strmv",flexiblas_time_strmv[1],flexiblas_call_strmv[1], (flexiblas_strmv.call_cblas==NULL&&flexiblas_call_strmv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","strsm",flexiblas_time_strsm[0],flexiblas_call_strsm[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_strsm",flexiblas_time_strsm[1],flexiblas_call_strsm[1], (flexiblas_strsm.call_cblas==NULL&&flexiblas_call_strsm[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","strsv",flexiblas_time_strsv[0],flexiblas_call_strsv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_strsv",flexiblas_time_strsv[1],flexiblas_call_strsv[1], (flexiblas_strsv.call_cblas==NULL&&flexiblas_call_strsv[1]>0)?"redirected to BLAS":"");
#endif

	
	fprintf(output, "\n");
	fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
	fprintf(output, "* Double Precission BLAS calls.                                               *\n"); 
	fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
	fprintf(output,"Function \t\t Runtime in s\t     Calls \n"); 
	fprintf(output,"%16s \t %11.7e \t %8lu\n","dasum",flexiblas_time_dasum[0],flexiblas_call_dasum[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dasum",flexiblas_time_dasum[1],flexiblas_call_dasum[1], (flexiblas_dasum.call_cblas==NULL&&flexiblas_call_dasum[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","daxpy",flexiblas_time_daxpy[0],flexiblas_call_daxpy[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_daxpy",flexiblas_time_daxpy[1],flexiblas_call_daxpy[1], (flexiblas_daxpy.call_cblas==NULL&&flexiblas_call_daxpy[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dcopy",flexiblas_time_dcopy[0],flexiblas_call_dcopy[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dcopy",flexiblas_time_dcopy[1],flexiblas_call_dcopy[1], (flexiblas_dcopy.call_cblas==NULL&&flexiblas_call_dcopy[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","ddot",flexiblas_time_ddot[0],flexiblas_call_ddot[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_ddot",flexiblas_time_ddot[1],flexiblas_call_ddot[1], (flexiblas_ddot.call_cblas==NULL&&flexiblas_call_ddot[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dgbmv",flexiblas_time_dgbmv[0],flexiblas_call_dgbmv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dgbmv",flexiblas_time_dgbmv[1],flexiblas_call_dgbmv[1], (flexiblas_dgbmv.call_cblas==NULL&&flexiblas_call_dgbmv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dgemm",flexiblas_time_dgemm[0],flexiblas_call_dgemm[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dgemm",flexiblas_time_dgemm[1],flexiblas_call_dgemm[1], (flexiblas_dgemm.call_cblas==NULL&&flexiblas_call_dgemm[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dgemv",flexiblas_time_dgemv[0],flexiblas_call_dgemv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dgemv",flexiblas_time_dgemv[1],flexiblas_call_dgemv[1], (flexiblas_dgemv.call_cblas==NULL&&flexiblas_call_dgemv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dger",flexiblas_time_dger[0],flexiblas_call_dger[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dger",flexiblas_time_dger[1],flexiblas_call_dger[1], (flexiblas_dger.call_cblas==NULL&&flexiblas_call_dger[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dnrm2",flexiblas_time_dnrm2[0],flexiblas_call_dnrm2[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dnrm2",flexiblas_time_dnrm2[1],flexiblas_call_dnrm2[1], (flexiblas_dnrm2.call_cblas==NULL&&flexiblas_call_dnrm2[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","drot",flexiblas_time_drot[0],flexiblas_call_drot[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_drot",flexiblas_time_drot[1],flexiblas_call_drot[1], (flexiblas_drot.call_cblas==NULL&&flexiblas_call_drot[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","drotg",flexiblas_time_drotg[0],flexiblas_call_drotg[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_drotg",flexiblas_time_drotg[1],flexiblas_call_drotg[1], (flexiblas_drotg.call_cblas==NULL&&flexiblas_call_drotg[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","drotm",flexiblas_time_drotm[0],flexiblas_call_drotm[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_drotm",flexiblas_time_drotm[1],flexiblas_call_drotm[1], (flexiblas_drotm.call_cblas==NULL&&flexiblas_call_drotm[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","drotmg",flexiblas_time_drotmg[0],flexiblas_call_drotmg[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_drotmg",flexiblas_time_drotmg[1],flexiblas_call_drotmg[1], (flexiblas_drotmg.call_cblas==NULL&&flexiblas_call_drotmg[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dsbmv",flexiblas_time_dsbmv[0],flexiblas_call_dsbmv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dsbmv",flexiblas_time_dsbmv[1],flexiblas_call_dsbmv[1], (flexiblas_dsbmv.call_cblas==NULL&&flexiblas_call_dsbmv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dscal",flexiblas_time_dscal[0],flexiblas_call_dscal[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dscal",flexiblas_time_dscal[1],flexiblas_call_dscal[1], (flexiblas_dscal.call_cblas==NULL&&flexiblas_call_dscal[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dspmv",flexiblas_time_dspmv[0],flexiblas_call_dspmv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dspmv",flexiblas_time_dspmv[1],flexiblas_call_dspmv[1], (flexiblas_dspmv.call_cblas==NULL&&flexiblas_call_dspmv[1]>0)?"redirected to BLAS":"");
#endif
	
	fprintf(output,"%16s \t %11.7e \t %8lu\n","dspr",flexiblas_time_dspr[0],flexiblas_call_dspr[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dspr",flexiblas_time_dspr[1],flexiblas_call_dspr[1], (flexiblas_dspr.call_cblas==NULL&&flexiblas_call_dspr[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dspr2",flexiblas_time_dspr2[0],flexiblas_call_dspr2[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dspr2",flexiblas_time_dspr2[1],flexiblas_call_dspr2[1], (flexiblas_dspr2.call_cblas==NULL&&flexiblas_call_dspr2[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dswap",flexiblas_time_dswap[0],flexiblas_call_dswap[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dswap",flexiblas_time_dswap[1],flexiblas_call_dswap[1], (flexiblas_dswap.call_cblas==NULL&&flexiblas_call_dswap[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dsymm",flexiblas_time_dsymm[0],flexiblas_call_dsymm[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dsymm",flexiblas_time_dsymm[1],flexiblas_call_dsymm[1], (flexiblas_dsymm.call_cblas==NULL&&flexiblas_call_dsymm[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dsymv",flexiblas_time_dsymv[0],flexiblas_call_dsymv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dsymv",flexiblas_time_dsymv[1],flexiblas_call_dsymv[1], (flexiblas_dsymv.call_cblas==NULL&&flexiblas_call_dsymv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dsyr",flexiblas_time_dsyr[0],flexiblas_call_dsyr[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dsyr",flexiblas_time_dsyr[1],flexiblas_call_dsyr[1], (flexiblas_dsyr.call_cblas==NULL&&flexiblas_call_dsyr[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dsyr2",flexiblas_time_dsyr2[0],flexiblas_call_dsyr2[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dsyr2",flexiblas_time_dsyr2[1],flexiblas_call_dsyr2[1], (flexiblas_dsyr2.call_cblas==NULL&&flexiblas_call_dsyr2[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dsyr2k",flexiblas_time_dsyr2k[0],flexiblas_call_dsyr2k[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dsyr2k",flexiblas_time_dsyr2k[1],flexiblas_call_dsyr2k[1], (flexiblas_dsyr2k.call_cblas==NULL&&flexiblas_call_dsyr2k[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dsyrk",flexiblas_time_dsyrk[0],flexiblas_call_dsyrk[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dsyrk",flexiblas_time_dsyrk[1],flexiblas_call_dsyrk[1], (flexiblas_dsyrk.call_cblas==NULL&&flexiblas_call_dsyrk[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dtbmv",flexiblas_time_dtbmv[0],flexiblas_call_dtbmv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dtbmv",flexiblas_time_dtbmv[1],flexiblas_call_dtbmv[1], (flexiblas_dtbmv.call_cblas==NULL&&flexiblas_call_dtbmv[1]>0)?"redirected to BLAS":"");
#endif


	fprintf(output,"%16s \t %11.7e \t %8lu\n","dtbsv",flexiblas_time_dtbsv[0],flexiblas_call_dtbsv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dtbsv",flexiblas_time_dtbsv[1],flexiblas_call_dtbsv[1], (flexiblas_dtbsv.call_cblas==NULL&&flexiblas_call_dtbsv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dtpmv",flexiblas_time_dtpmv[0],flexiblas_call_dtpmv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dtpmv",flexiblas_time_dtpmv[1],flexiblas_call_dtpmv[1], (flexiblas_dtpmv.call_cblas==NULL&&flexiblas_call_dtpmv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dtpsv",flexiblas_time_dtpsv[0],flexiblas_call_dtpsv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dtpsv",flexiblas_time_dtpsv[1],flexiblas_call_dtpsv[1], (flexiblas_dtpsv.call_cblas==NULL&&flexiblas_call_dtpsv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dtrmm",flexiblas_time_dtrmm[0],flexiblas_call_dtrmm[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dtrmm",flexiblas_time_dtrmm[1],flexiblas_call_dtrmm[1], (flexiblas_dtrmm.call_cblas==NULL&&flexiblas_call_dtrmm[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dtrmv",flexiblas_time_dtrmv[0],flexiblas_call_dtrmv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dtrmv",flexiblas_time_dtrmv[1],flexiblas_call_dtrmv[1], (flexiblas_dtrmv.call_cblas==NULL&&flexiblas_call_dtrmv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dtrsm",flexiblas_time_dtrsm[0],flexiblas_call_dtrsm[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dtrsm",flexiblas_time_dtrsm[1],flexiblas_call_dtrsm[1], (flexiblas_dtrsm.call_cblas==NULL&&flexiblas_call_dtrsm[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dtrsv",flexiblas_time_dtrsv[0],flexiblas_call_dtrsv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dtrsv",flexiblas_time_dtrsv[1],flexiblas_call_dtrsv[1], (flexiblas_dtrsv.call_cblas==NULL&&flexiblas_call_dtrsv[1]>0)?"redirected to BLAS":"");
#endif

	
	fprintf(output, "\n");
	fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
	fprintf(output, "* Complex Single Precission BLAS calls.                                       *\n"); 
	fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
	fprintf(output,"Function \t\t Runtime in s\t     Calls \n"); 
	fprintf(output,"%16s \t %11.7e \t %8lu\n","caxpy",flexiblas_time_caxpy[0],flexiblas_call_caxpy[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_caxpy",flexiblas_time_caxpy[1],flexiblas_call_caxpy[1], (flexiblas_caxpy.call_cblas==NULL&&flexiblas_call_caxpy[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","ccopy",flexiblas_time_ccopy[0],flexiblas_call_ccopy[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_ccopy",flexiblas_time_ccopy[1],flexiblas_call_ccopy[1], (flexiblas_ccopy.call_cblas==NULL&&flexiblas_call_ccopy[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","cdotc",flexiblas_time_cdotc[0],flexiblas_call_cdotc[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_cdotc",flexiblas_time_cdotc[1],flexiblas_call_cdotc[1], (flexiblas_cdotc.call_cblas==NULL&&flexiblas_call_cdotc[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","cdotu",flexiblas_time_cdotu[0],flexiblas_call_cdotu[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_cdotu",flexiblas_time_cdotu[1],flexiblas_call_cdotu[1], (flexiblas_cdotu.call_cblas==NULL&&flexiblas_call_cdotu[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","cgbmv",flexiblas_time_cgbmv[0],flexiblas_call_cgbmv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_cgbmv",flexiblas_time_cgbmv[1],flexiblas_call_cgbmv[1], (flexiblas_cgbmv.call_cblas==NULL&&flexiblas_call_cgbmv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","cgemm",flexiblas_time_cgemm[0],flexiblas_call_cgemm[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_cgemm",flexiblas_time_cgemm[1],flexiblas_call_cgemm[1], (flexiblas_cgemm.call_cblas==NULL&&flexiblas_call_cgemm[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","cgemv",flexiblas_time_cgemv[0],flexiblas_call_cgemv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_cgemv",flexiblas_time_cgemv[1],flexiblas_call_cgemv[1], (flexiblas_cgemv.call_cblas==NULL&&flexiblas_call_cgemv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","cgerc",flexiblas_time_cgerc[0],flexiblas_call_cgerc[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_cgerc",flexiblas_time_cgerc[1],flexiblas_call_cgerc[1], (flexiblas_cgerc.call_cblas==NULL&&flexiblas_call_cgerc[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","cgeru",flexiblas_time_cgeru[0],flexiblas_call_cgeru[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_cgeru",flexiblas_time_cgeru[1],flexiblas_call_cgeru[1], (flexiblas_cgeru.call_cblas==NULL&&flexiblas_call_cgeru[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","chbmv",flexiblas_time_chbmv[0],flexiblas_call_chbmv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_chbmv",flexiblas_time_chbmv[1],flexiblas_call_chbmv[1], (flexiblas_chbmv.call_cblas==NULL&&flexiblas_call_chbmv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","chemm",flexiblas_time_chemm[0],flexiblas_call_chemm[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_chemm",flexiblas_time_chemm[1],flexiblas_call_chemm[1], (flexiblas_chemm.call_cblas==NULL&&flexiblas_call_chemm[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","chemv",flexiblas_time_chemv[0],flexiblas_call_chemv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_chemv",flexiblas_time_chemv[1],flexiblas_call_chemv[1], (flexiblas_chemv.call_cblas==NULL&&flexiblas_call_chemv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","cher",flexiblas_time_cher[0],flexiblas_call_cher[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_cher",flexiblas_time_cher[1],flexiblas_call_cher[1], (flexiblas_cher.call_cblas==NULL&&flexiblas_call_cher[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","cher2",flexiblas_time_cher2[0],flexiblas_call_cher2[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_cher2",flexiblas_time_cher2[1],flexiblas_call_cher2[1], (flexiblas_cher2.call_cblas==NULL&&flexiblas_call_cher2[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","cher2k",flexiblas_time_cher2k[0],flexiblas_call_cher2k[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_cher2k",flexiblas_time_cher2k[1],flexiblas_call_cher2k[1], (flexiblas_cher2k.call_cblas==NULL&&flexiblas_call_cher2k[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","cherk",flexiblas_time_cherk[0],flexiblas_call_cherk[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_cherk",flexiblas_time_cherk[1],flexiblas_call_cherk[1], (flexiblas_cherk.call_cblas==NULL&&flexiblas_call_cherk[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","chpmv",flexiblas_time_chpmv[0],flexiblas_call_chpmv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_chpmv",flexiblas_time_chpmv[1],flexiblas_call_chpmv[1], (flexiblas_chpmv.call_cblas==NULL&&flexiblas_call_chpmv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","chpr",flexiblas_time_chpr[0],flexiblas_call_chpr[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_chpr",flexiblas_time_chpr[1],flexiblas_call_chpr[1], (flexiblas_chpr.call_cblas==NULL&&flexiblas_call_chpr[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","chpr2",flexiblas_time_chpr2[0],flexiblas_call_chpr2[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_chpr2",flexiblas_time_chpr2[1],flexiblas_call_chpr2[1], (flexiblas_chpr2.call_cblas==NULL&&flexiblas_call_chpr2[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","crotg",flexiblas_time_crotg[0],flexiblas_call_crotg[0]);
	fprintf(output,"%16s \t %11.7e \t %8lu\n","csrot",flexiblas_time_csrot[0],flexiblas_call_csrot[0]);

	fprintf(output,"%16s \t %11.7e \t %8lu\n","cscal",flexiblas_time_cscal[0],flexiblas_call_cscal[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_cscal",flexiblas_time_cscal[1],flexiblas_call_cscal[1], (flexiblas_cscal.call_cblas==NULL&&flexiblas_call_cscal[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","csscal",flexiblas_time_csscal[0],flexiblas_call_csscal[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_csscal",flexiblas_time_csscal[1],flexiblas_call_csscal[1], (flexiblas_csscal.call_cblas==NULL&&flexiblas_call_csscal[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","cswap",flexiblas_time_cswap[0],flexiblas_call_cswap[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_cswap",flexiblas_time_cswap[1],flexiblas_call_cswap[1], (flexiblas_cswap.call_cblas==NULL&&flexiblas_call_cswap[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","csymm",flexiblas_time_csymm[0],flexiblas_call_csymm[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_csymm",flexiblas_time_csymm[1],flexiblas_call_csymm[1], (flexiblas_csymm.call_cblas==NULL&&flexiblas_call_csymm[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","csyr2k",flexiblas_time_csyr2k[0],flexiblas_call_csyr2k[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_csyr2k",flexiblas_time_csyr2k[1],flexiblas_call_csyr2k[1], (flexiblas_csyr2k.call_cblas==NULL&&flexiblas_call_csyr2k[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","csyrk",flexiblas_time_csyrk[0],flexiblas_call_csyrk[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_csyrk",flexiblas_time_csyrk[1],flexiblas_call_csyrk[1], (flexiblas_csyrk.call_cblas==NULL&&flexiblas_call_csyrk[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","ctbmv",flexiblas_time_ctbmv[0],flexiblas_call_ctbmv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_ctbmv",flexiblas_time_ctbmv[1],flexiblas_call_ctbmv[1], (flexiblas_ctbmv.call_cblas==NULL&&flexiblas_call_ctbmv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","ctbsv",flexiblas_time_ctbsv[0],flexiblas_call_ctbsv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_ctbsv",flexiblas_time_ctbsv[1],flexiblas_call_ctbsv[1], (flexiblas_ctbsv.call_cblas==NULL&&flexiblas_call_ctbsv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","ctpmv",flexiblas_time_ctpmv[0],flexiblas_call_ctpmv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_ctpmv",flexiblas_time_ctpmv[1],flexiblas_call_ctpmv[1], (flexiblas_ctpmv.call_cblas==NULL&&flexiblas_call_ctpmv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","ctpsv",flexiblas_time_ctpsv[0],flexiblas_call_ctpsv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_ctpsv",flexiblas_time_ctpsv[1],flexiblas_call_ctpsv[1], (flexiblas_ctpsv.call_cblas==NULL&&flexiblas_call_ctpsv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","ctrmm",flexiblas_time_ctrmm[0],flexiblas_call_ctrmm[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_ctrmm",flexiblas_time_ctrmm[1],flexiblas_call_ctrmm[1], (flexiblas_ctrmm.call_cblas==NULL&&flexiblas_call_ctrmm[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","ctrmv",flexiblas_time_ctrmv[0],flexiblas_call_ctrmv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_ctrmv",flexiblas_time_ctrmv[1],flexiblas_call_ctrmv[1], (flexiblas_ctrmv.call_cblas==NULL&&flexiblas_call_ctrmv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","ctrsm",flexiblas_time_ctrsm[0],flexiblas_call_ctrsm[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_ctrsm",flexiblas_time_ctrsm[1],flexiblas_call_ctrsm[1], (flexiblas_ctrsm.call_cblas==NULL&&flexiblas_call_ctrsm[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","ctrsv",flexiblas_time_ctrsv[0],flexiblas_call_ctrsv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_ctrsv",flexiblas_time_ctrsv[1],flexiblas_call_ctrsv[1], (flexiblas_ctrsv.call_cblas==NULL&&flexiblas_call_ctrsv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","scasum",flexiblas_time_scasum[0],flexiblas_call_scasum[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_scasum",flexiblas_time_scasum[1],flexiblas_call_scasum[1], (flexiblas_scasum.call_cblas==NULL&&flexiblas_call_scasum[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","scnrm2",flexiblas_time_scnrm2[0],flexiblas_call_scnrm2[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_scnrm2",flexiblas_time_scnrm2[1],flexiblas_call_scnrm2[1], (flexiblas_scnrm2.call_cblas==NULL&&flexiblas_call_scnrm2[1]>0)?"redirected to BLAS":"");
#endif



	fprintf(output, "\n");
	fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
	fprintf(output, "* Complex Double Precission BLAS calls.                                       *\n"); 
	fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
	fprintf(output,"Function \t\t Runtime in s\t     Calls \n"); 
	fprintf(output,"%16s \t %11.7e \t %8lu\n","zaxpy",flexiblas_time_zaxpy[0],flexiblas_call_zaxpy[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zaxpy",flexiblas_time_zaxpy[1],flexiblas_call_zaxpy[1], (flexiblas_zaxpy.call_cblas==NULL&&flexiblas_call_zaxpy[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","zcopy",flexiblas_time_zcopy[0],flexiblas_call_zcopy[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zcopy",flexiblas_time_zcopy[1],flexiblas_call_zcopy[1], (flexiblas_zcopy.call_cblas==NULL&&flexiblas_call_zcopy[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","zdotc",flexiblas_time_zdotc[0],flexiblas_call_zdotc[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zdotc",flexiblas_time_zdotc[1],flexiblas_call_zdotc[1], (flexiblas_zdotc.call_cblas==NULL&&flexiblas_call_zdotc[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","zdotu",flexiblas_time_zdotu[0],flexiblas_call_zdotu[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zdotu",flexiblas_time_zdotu[1],flexiblas_call_zdotu[1], (flexiblas_zdotu.call_cblas==NULL&&flexiblas_call_zdotu[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","zdscal",flexiblas_time_zdscal[0],flexiblas_call_zdscal[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zdscal",flexiblas_time_zdscal[1],flexiblas_call_zdscal[1], (flexiblas_zdscal.call_cblas==NULL&&flexiblas_call_zdscal[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","zgbmv",flexiblas_time_zgbmv[0],flexiblas_call_zgbmv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zgbmv",flexiblas_time_zgbmv[1],flexiblas_call_zgbmv[1], (flexiblas_zgbmv.call_cblas==NULL&&flexiblas_call_zgbmv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","zgemm",flexiblas_time_zgemm[0],flexiblas_call_zgemm[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zgemm",flexiblas_time_zgemm[1],flexiblas_call_zgemm[1], (flexiblas_zgemm.call_cblas==NULL&&flexiblas_call_zgemm[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","zgemv",flexiblas_time_zgemv[0],flexiblas_call_zgemv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zgemv",flexiblas_time_zgemv[1],flexiblas_call_zgemv[1], (flexiblas_zgemv.call_cblas==NULL&&flexiblas_call_zgemv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","zgerc",flexiblas_time_zgerc[0],flexiblas_call_zgerc[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zgerc",flexiblas_time_zgerc[1],flexiblas_call_zgerc[1], (flexiblas_zgerc.call_cblas==NULL&&flexiblas_call_zgerc[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","zgeru",flexiblas_time_zgeru[0],flexiblas_call_zgeru[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zgeru",flexiblas_time_zgeru[1],flexiblas_call_zgeru[1], (flexiblas_zgeru.call_cblas==NULL&&flexiblas_call_zgeru[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","zhbmv",flexiblas_time_zhbmv[0],flexiblas_call_zhbmv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zhbmv",flexiblas_time_zhbmv[1],flexiblas_call_zhbmv[1], (flexiblas_zhbmv.call_cblas==NULL&&flexiblas_call_zhbmv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","zhemm",flexiblas_time_zhemm[0],flexiblas_call_zhemm[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zhemm",flexiblas_time_zhemm[1],flexiblas_call_zhemm[1], (flexiblas_zhemm.call_cblas==NULL&&flexiblas_call_zhemm[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","zhemv",flexiblas_time_zhemv[0],flexiblas_call_zhemv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zhemv",flexiblas_time_zhemv[1],flexiblas_call_zhemv[1], (flexiblas_zhemv.call_cblas==NULL&&flexiblas_call_zhemv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","zher",flexiblas_time_zher[0],flexiblas_call_zher[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zher",flexiblas_time_zher[1],flexiblas_call_zher[1], (flexiblas_zher.call_cblas==NULL&&flexiblas_call_zher[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","zher2",flexiblas_time_zher2[0],flexiblas_call_zher2[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zher2",flexiblas_time_zher2[1],flexiblas_call_zher2[1], (flexiblas_zher2.call_cblas==NULL&&flexiblas_call_zher2[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","zher2k",flexiblas_time_zher2k[0],flexiblas_call_zher2k[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zher2k",flexiblas_time_zher2k[1],flexiblas_call_zher2k[1], (flexiblas_zher2k.call_cblas==NULL&&flexiblas_call_zher2k[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","zherk",flexiblas_time_zherk[0],flexiblas_call_zherk[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zherk",flexiblas_time_zherk[1],flexiblas_call_zherk[1], (flexiblas_zherk.call_cblas==NULL&&flexiblas_call_zherk[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","zhpmv",flexiblas_time_zhpmv[0],flexiblas_call_zhpmv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zhpmv",flexiblas_time_zhpmv[1],flexiblas_call_zhpmv[1], (flexiblas_zhpmv.call_cblas==NULL&&flexiblas_call_zhpmv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","zhpr",flexiblas_time_zhpr[0],flexiblas_call_zhpr[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zhpr",flexiblas_time_zhpr[1],flexiblas_call_zhpr[1], (flexiblas_zhpr.call_cblas==NULL&&flexiblas_call_zhpr[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","zhpr2",flexiblas_time_zhpr2[0],flexiblas_call_zhpr2[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zhpr2",flexiblas_time_zhpr2[1],flexiblas_call_zhpr2[1], (flexiblas_zhpr2.call_cblas==NULL&&flexiblas_call_zhpr2[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","zrotg",flexiblas_time_zrotg[0],flexiblas_call_zrotg[0]);
	fprintf(output,"%16s \t %11.7e \t %8lu\n","zdrot",flexiblas_time_zdrot[0],flexiblas_call_zdrot[0]);

	fprintf(output,"%16s \t %11.7e \t %8lu\n","zscal",flexiblas_time_zscal[0],flexiblas_call_zscal[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zscal",flexiblas_time_zscal[1],flexiblas_call_zscal[1], (flexiblas_zscal.call_cblas==NULL&&flexiblas_call_zscal[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","zswap",flexiblas_time_zswap[0],flexiblas_call_zswap[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zswap",flexiblas_time_zswap[1],flexiblas_call_zswap[1], (flexiblas_zswap.call_cblas==NULL&&flexiblas_call_zswap[1]>0)?"redirected to BLAS":"");
#endif

	
	fprintf(output,"%16s \t %11.7e \t %8lu\n","zsymm",flexiblas_time_zsymm[0],flexiblas_call_zsymm[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zsymm",flexiblas_time_zsymm[1],flexiblas_call_zsymm[1], (flexiblas_zsymm.call_cblas==NULL&&flexiblas_call_zsymm[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","zsyr2k",flexiblas_time_zsyr2k[0],flexiblas_call_zsyr2k[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zsyr2k",flexiblas_time_zsyr2k[1],flexiblas_call_zsyr2k[1], (flexiblas_zsyr2k.call_cblas==NULL&&flexiblas_call_zsyr2k[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","zsyrk",flexiblas_time_zsyrk[0],flexiblas_call_zsyrk[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zsyrk",flexiblas_time_zsyrk[1],flexiblas_call_zsyrk[1], (flexiblas_zsyrk.call_cblas==NULL&&flexiblas_call_zsyrk[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","ztbmv",flexiblas_time_ztbmv[0],flexiblas_call_ztbmv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_ztbmv",flexiblas_time_ztbmv[1],flexiblas_call_ztbmv[1], (flexiblas_ztbmv.call_cblas==NULL&&flexiblas_call_ztbmv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","ztbsv",flexiblas_time_ztbsv[0],flexiblas_call_ztbsv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_ztbsv",flexiblas_time_ztbsv[1],flexiblas_call_ztbsv[1], (flexiblas_ztbsv.call_cblas==NULL&&flexiblas_call_ztbsv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","ztpmv",flexiblas_time_ztpmv[0],flexiblas_call_ztpmv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_ztpmv",flexiblas_time_ztpmv[1],flexiblas_call_ztpmv[1], (flexiblas_ztpmv.call_cblas==NULL&&flexiblas_call_ztpmv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","ztpsv",flexiblas_time_ztpsv[0],flexiblas_call_ztpsv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_ztpsv",flexiblas_time_ztpsv[1],flexiblas_call_ztpsv[1], (flexiblas_ztpsv.call_cblas==NULL&&flexiblas_call_ztpsv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","ztrmm",flexiblas_time_ztrmm[0],flexiblas_call_ztrmm[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_ztrmm",flexiblas_time_ztrmm[1],flexiblas_call_ztrmm[1], (flexiblas_ztrmm.call_cblas==NULL&&flexiblas_call_ztrmm[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","ztrmv",flexiblas_time_ztrmv[0],flexiblas_call_ztrmv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_ztrmv",flexiblas_time_ztrmv[1],flexiblas_call_ztrmv[1], (flexiblas_ztrmv.call_cblas==NULL&&flexiblas_call_ztrmv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","ztrsm",flexiblas_time_ztrsm[0],flexiblas_call_ztrsm[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_ztrsm",flexiblas_time_ztrsm[1],flexiblas_call_ztrsm[1], (flexiblas_ztrsm.call_cblas==NULL&&flexiblas_call_ztrsm[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","ztrsv",flexiblas_time_ztrsv[0],flexiblas_call_ztrsv[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_ztrsv",flexiblas_time_ztrsv[1],flexiblas_call_ztrsv[1], (flexiblas_ztrsv.call_cblas==NULL&&flexiblas_call_ztrsv[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dzasum",flexiblas_time_dzasum[0],flexiblas_call_dzasum[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dzasum",flexiblas_time_dzasum[1],flexiblas_call_dzasum[1], (flexiblas_dzasum.call_cblas==NULL&&flexiblas_call_dzasum[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","dznrm2",flexiblas_time_dznrm2[0],flexiblas_call_dznrm2[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dznrm2",flexiblas_time_dznrm2[1],flexiblas_call_dznrm2[1], (flexiblas_dznrm2.call_cblas==NULL&&flexiblas_call_dznrm2[1]>0)?"redirected to BLAS":"");
#endif
	/* BLAS Extension  */
#ifdef EXTBLAS
	fprintf(output, "\n");
	fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
	fprintf(output, "* BLAS Extension calls.                                                       *\n"); 
	fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
	fprintf(output,"Function \t\t Runtime in s\t     Calls \n"); 

	fprintf(output,"%16s \t %11.7e \t %8lu\n","saxpby",flexiblas_time_saxpby[0],flexiblas_call_saxpby[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_saxpby",flexiblas_time_saxpby[1],flexiblas_call_saxpby[1], (flexiblas_saxpby.call_cblas==NULL&&flexiblas_call_saxpby[1]>0)?"redirected to BLAS":"");
#endif
	fprintf(output,"%16s \t %11.7e \t %8lu\n","daxpby",flexiblas_time_daxpby[0],flexiblas_call_daxpby[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_daxpby",flexiblas_time_daxpby[1],flexiblas_call_daxpby[1], (flexiblas_daxpby.call_cblas==NULL&&flexiblas_call_daxpby[1]>0)?"redirected to BLAS":"");
#endif
	fprintf(output,"%16s \t %11.7e \t %8lu\n","caxpby",flexiblas_time_caxpby[0],flexiblas_call_caxpby[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_caxpby",flexiblas_time_caxpby[1],flexiblas_call_caxpby[1], (flexiblas_caxpby.call_cblas==NULL&&flexiblas_call_caxpby[1]>0)?"redirected to BLAS":"");
#endif
	fprintf(output,"%16s \t %11.7e \t %8lu\n","zaxpby",flexiblas_time_zaxpby[0],flexiblas_call_zaxpby[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zaxpby",flexiblas_time_zaxpby[1],flexiblas_call_zaxpby[1], (flexiblas_zaxpby.call_cblas==NULL&&flexiblas_call_zaxpby[1]>0)?"redirected to BLAS":"");
#endif
	fprintf(output,"%16s \t %11.7e \t %8lu\n","somatcopy",flexiblas_time_somatcopy[0],flexiblas_call_somatcopy[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_somatcopy",flexiblas_time_somatcopy[1],flexiblas_call_somatcopy[1], (flexiblas_somatcopy.call_cblas==NULL&&flexiblas_call_somatcopy[1]>0)?"redirected to BLAS":"");
#endif
	fprintf(output,"%16s \t %11.7e \t %8lu\n","domatcopy",flexiblas_time_domatcopy[0],flexiblas_call_domatcopy[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_domatcopy",flexiblas_time_domatcopy[1],flexiblas_call_domatcopy[1], (flexiblas_domatcopy.call_cblas==NULL&&flexiblas_call_domatcopy[1]>0)?"redirected to BLAS":"");
#endif
	fprintf(output,"%16s \t %11.7e \t %8lu\n","comatcopy",flexiblas_time_comatcopy[0],flexiblas_call_comatcopy[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_comatcopy",flexiblas_time_comatcopy[1],flexiblas_call_comatcopy[1], (flexiblas_comatcopy.call_cblas==NULL&&flexiblas_call_comatcopy[1]>0)?"redirected to BLAS":"");
#endif
	fprintf(output,"%16s \t %11.7e \t %8lu\n","zomatcopy",flexiblas_time_zomatcopy[0],flexiblas_call_zomatcopy[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zomatcopy",flexiblas_time_zomatcopy[1],flexiblas_call_zomatcopy[1], (flexiblas_zomatcopy.call_cblas==NULL&&flexiblas_call_zomatcopy[1]>0)?"redirected to BLAS":"");
#endif

	fprintf(output,"%16s \t %11.7e \t %8lu\n","simatcopy",flexiblas_time_simatcopy[0],flexiblas_call_simatcopy[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_simatcopy",flexiblas_time_simatcopy[1],flexiblas_call_simatcopy[1], (flexiblas_simatcopy.call_cblas==NULL&&flexiblas_call_simatcopy[1]>0)?"redirected to BLAS":"");
#endif
	fprintf(output,"%16s \t %11.7e \t %8lu\n","dimatcopy",flexiblas_time_dimatcopy[0],flexiblas_call_dimatcopy[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_dimatcopy",flexiblas_time_dimatcopy[1],flexiblas_call_dimatcopy[1], (flexiblas_dimatcopy.call_cblas==NULL&&flexiblas_call_dimatcopy[1]>0)?"redirected to BLAS":"");
#endif
	fprintf(output,"%16s \t %11.7e \t %8lu\n","cimatcopy",flexiblas_time_cimatcopy[0],flexiblas_call_cimatcopy[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_cimatcopy",flexiblas_time_cimatcopy[1],flexiblas_call_cimatcopy[1], (flexiblas_cimatcopy.call_cblas==NULL&&flexiblas_call_cimatcopy[1]>0)?"redirected to BLAS":"");
#endif
	fprintf(output,"%16s \t %11.7e \t %8lu\n","zimatcopy",flexiblas_time_zimatcopy[0],flexiblas_call_zimatcopy[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n","cblas_zimatcopy",flexiblas_time_zimatcopy[1],flexiblas_call_zimatcopy[1], (flexiblas_zimatcopy.call_cblas==NULL&&flexiblas_call_zimatcopy[1]>0)?"redirected to BLAS":"");
#endif


#endif 

	fprintf(output, "*******************************************************************************\n"); 

	fprintf(output,"%16s \t %11.7e \t %8lu\n","xerbla",flexiblas_time_xerbla[0],flexiblas_call_xerbla[0]);
#ifdef FLEXIBLAS_CBLAS
	fprintf(output,"%16s \t %11.7e \t %8lu\n","cblas_xerbla",flexiblas_time_xerbla[1],flexiblas_call_xerbla[1]);
#endif

	fprintf(output, "*******************************************************************************\n"); 


	if ( output != stderr) fclose(output); 
	return; 
}

double flexiblas_wtime ()
{
	struct timeval tv;
	gettimeofday (&tv, NULL);
	return tv.tv_sec + tv.tv_usec / 1e6;
}

#ifdef __WIN32__ 
#include <windows.h>
BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved)
{
  switch (fdwReason)
  {
    case DLL_PROCESS_ATTACH:
      /* Code path executed when DLL is loaded into a process's address space. */
      flexiblas_init(); 
      break;

    case DLL_THREAD_ATTACH:
      /* Code path executed when a new thread is created within the process. */
      break;

    case DLL_THREAD_DETACH:
      /* Code path executed when a thread within the process has exited *cleanly*. */
      break;

    case DLL_PROCESS_DETACH:
      /* Code path executed when DLL is unloaded from a process's address space. */
      flexiblas_exit(); 
      break;
  }

  return TRUE;
}
#endif
