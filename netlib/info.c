/* $Id$ */
/**
 * @file info.c
 * @brief <+brief+> 
 * @author Martin Köhler
 * @version $Id$ 
 *
 * This file implelemts <+details+>
 */
#include "../src/flexiblas_info.h" 

void __flexiblas_info(struct flexiblas_info * info) {
#ifdef __ICC
	info -> zdotc_is_intel = 1; 
	info -> zdotu_is_intel = 1; 
	info -> cdotc_is_intel = 1; 
	info -> cdotu_is_intel = 1; 
#else 
	info -> zdotc_is_intel = 0; 
	info -> zdotu_is_intel = 0; 
	info -> cdotc_is_intel = 0; 
	info -> cdotu_is_intel = 0; 
#endif 

	info -> scabs1_missing = 0; 
}



