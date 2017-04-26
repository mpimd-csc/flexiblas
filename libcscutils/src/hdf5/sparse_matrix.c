/* 
* CSCUTILS - A collection of various software routines uses in CSC projects
* Copyright (C) 2015 Martin Koehler
* 
* This library is free software; you can redistribute it and/or modify
* it under the terms of the GNU Lesser General Public License as published
* by the Free Software Foundation; either version 2.1 of the License, or
* (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public License
* along with this library; if not, see <http://www.gnu.org/licenses/>.
* 
*/ 

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <complex.h>
#include <string.h>

#include <hdf5.h>
#include <hdf5_hl.h>

#include "cscutils/hdf.h"
#include "csc_hdf5_common.h"

#define MAX(A,B) (((A)>(B))?(A):(B))

int csc_hdf5_sparse_size(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz) 
{
    if (    csc_hdf5_is_spcsr(root,dset_name) 
            ||  csc_hdf5_is_spcsc(root,dset_name) 
            ||  csc_hdf5_is_spcoo(root,dset_name) ) 
    {
        HDF5_GET_ATTR_ULONG(root,dset_name,"rows",*rows); 
        HDF5_GET_ATTR_ULONG(root,dset_name,"cols",*cols); 
        HDF5_GET_ATTR_ULONG(root,dset_name,"nnz",*nnz); 

        return 0; 
    } else {
        return -1; 
    }
    return 0; 
}

csc_hdf5_content_t csc_hdf5_sparse_content(hid_t root, const char *dset_name) {
    if ( csc_hdf5_is_spcsr ( root, dset_name) ) {
        return CSC_HDF5_SPARSE_CSR; 
    } else if ( csc_hdf5_is_spcsc(root, dset_name)) {
        return CSC_HDF5_SPARSE_CSC; 
    } else if ( csc_hdf5_is_spcoo(root, dset_name)) {
        return CSC_HDF5_SPARSE_COORDINATE; 
    } else {
        return CSC_HDF5_UNKOWN_CONTENT; 
    }
    return CSC_HDF5_UNKOWN_CONTENT; 
}

/* Write a coordinate Real matrix with 4 byte integers   */
int csc_hdf5_sparse_write (csc_hdf5_content_t type, csc_hdf5_field_t number_field, csc_hdf5_field_t integer_field, hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz, void *rowptr, void *colptr, void *values) 
{
    herr_t err;  
    hid_t sparse_group = -1; 
    unsigned long ul_rows, ul_cols, ul_nnz; 
    hsize_t dims[1]; 
    hid_t integer_type; 


    /*-----------------------------------------------------------------------------
     *  Select Int type
     *-----------------------------------------------------------------------------*/
    if ( integer_field == CSC_HDF5_INTEGER8 ) {
        integer_type = H5T_NATIVE_CHAR;
    } else if ( integer_field == CSC_HDF5_INTEGER16 ) {
        integer_type = H5T_NATIVE_SHORT; 
    } else if ( integer_field == CSC_HDF5_INTEGER32 ) {
        integer_type = H5T_NATIVE_INT; 
    } else if ( integer_field == CSC_HDF5_INTEGER64 ) {
        integer_type = H5T_NATIVE_LONG; 
    } else {
        csc_error_message("Unsupported Integer type.\n"); 
        return -1; 
    }
    
    
    if ( H5Lexists(root, dset_name, H5P_DEFAULT) > 0 ) {
        if ( H5Ldelete(root, dset_name, H5P_DEFAULT) < 0 ) {
            csc_error_message("Failed to remove previous entry %s\n", dset_name); 
            return -1; 
        }
    }
	sparse_group = H5Gcreate2(root,dset_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT); 
    if (sparse_group < 0 ) {
        return -1; 
    }

    
    /*-----------------------------------------------------------------------------
     *  Add attributes to Group
     *-----------------------------------------------------------------------------*/
    if ( type == CSC_HDF5_SPARSE_COORDINATE ) {
    	err = csc_hdf5_set_content(root, dset_name, CSC_HDF5_SPARSE_COORDINATE); 
    } else if ( type == CSC_HDF5_SPARSE_CSC ) {
        err = csc_hdf5_set_content(root, dset_name, CSC_HDF5_SPARSE_CSC); 
    } else if ( type == CSC_HDF5_SPARSE_CSR ) {
    	err = csc_hdf5_set_content(root, dset_name, CSC_HDF5_SPARSE_CSR); 
    } else {
        csc_error_message("Content type not supported for sparse matrices.\n"); 
        H5Gclose(sparse_group); 
        return -1; 
    }

	if ( err < 0 ) { 
		csc_error_message("Failed to set attribute content_type.\n"); 
        H5Gclose(sparse_group); 
		return -1; 
	}
    err = csc_hdf5_set_field(root,dset_name,number_field);  
    if ( err < 0 ) { 
        csc_error_message("Failed to set the field type.\n"); 
        return -1; 
    }

    ul_cols = (unsigned long) cols; 
    ul_rows = (unsigned long) rows; 
    ul_nnz  = (unsigned long) nnz; 

    err = H5LTset_attribute_ulong(root, dset_name, "rows", &ul_rows, 1); 
	if ( err < 0 ) {
		csc_error_message("Failed to set attribute rows.\n"); 
        H5Gclose(sparse_group); 
		return -1; 
	}
    
    err = H5LTset_attribute_ulong(root, dset_name, "cols", &ul_cols, 1); 
	if ( err < 0 ) {
		csc_error_message("Failed to set attribute cols.\n"); 
        H5Gclose(sparse_group); 
		return -1; 
	}

    err = H5LTset_attribute_ulong(root, dset_name, "nnz", &ul_nnz, 1); 
	if ( err < 0 ) {
		csc_error_message("Failed to set attribute nnz.\n"); 
        H5Gclose(sparse_group); 
		return -1; 
	}


    /*-----------------------------------------------------------------------------
     *  Write Data 
     *-----------------------------------------------------------------------------*/
    if ( type == CSC_HDF5_SPARSE_COORDINATE ) {
        dims[0] = nnz; 
        err = csc_hdf5_create_dataset(sparse_group, "rowptr", 1, dims, integer_type, rowptr); 
        if ( err < 0 ) {
            csc_error_message("Failed to set rowptr data.\n"); 
            H5Gclose(sparse_group); 
            return -1; 
        }

        dims[0] = nnz; 
        err = csc_hdf5_create_dataset(sparse_group, "colptr", 1, dims, integer_type, colptr); 
        if ( err < 0 ) {
            csc_error_message("Failed to set colptr data.\n"); 
            H5Gclose(sparse_group); 
            return -1; 
        }
    } else if ( type == CSC_HDF5_SPARSE_CSC ) {
        dims[0] = nnz; 
        err = csc_hdf5_create_dataset(sparse_group, "rowptr", 1, dims, integer_type, rowptr); 
        if ( err < 0 ) {
            csc_error_message("Failed to set rowptr data.\n"); 
            H5Gclose(sparse_group); 
            return -1; 
        }

        dims[0] = cols + 1; 
        err = csc_hdf5_create_dataset(sparse_group, "colptr", 1, dims, integer_type, colptr); 
        if ( err < 0 ) {
            csc_error_message("Failed to set colptr data.\n"); 
            H5Gclose(sparse_group); 
            return -1; 
        }

    } else if ( type == CSC_HDF5_SPARSE_CSR ) {
        dims[0] = rows + 1; 
        err = csc_hdf5_create_dataset(sparse_group, "rowptr", 1, dims, integer_type, rowptr); 
        if ( err < 0 ) {
            csc_error_message("Failed to set rowptr data.\n"); 
            H5Gclose(sparse_group); 
            return -1; 
        }

        dims[0] = nnz; 
        err = csc_hdf5_create_dataset(sparse_group, "colptr", 1, dims, integer_type, colptr); 
        if ( err < 0 ) {
            csc_error_message("Failed to set colptr data.\n"); 
            H5Gclose(sparse_group); 
            return -1; 
        }
    }

    dims[0] = nnz; 
    if ( number_field == CSC_HDF5_REAL ) {
    	err = csc_hdf5_create_dataset(sparse_group, "values", 1, dims, H5T_NATIVE_DOUBLE, values); 
    } else if ( number_field == CSC_HDF5_COMPLEX ) {
        hid_t complex_type; 
        complex_type = csc_hdf5_complex_type(root); 
    	if ( complex_type < 0 ) {
	    	csc_error_message("Failed to create complex type.\n"); 
    		return 0; 
	    }
    	err = csc_hdf5_create_dataset(sparse_group, "values", 1, dims, complex_type, values); 
        H5Tclose(complex_type); 
    }

    if ( err < 0 ) {
		csc_error_message("Failed to set values data.\n"); 
        H5Gclose(sparse_group); 
        return -1; 
    }

    H5Gclose(sparse_group); 

    return 0; 
}



/* Write a coordinate Real matrix with 4 byte integers   */
int csc_hdf5_sparse_read (csc_hdf5_content_t *type, csc_hdf5_field_t *number_field, csc_hdf5_field_t integer_field, hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz, void *rowptr, void *colptr, void *values) 
{
    herr_t err;  
    hid_t sparse_group = -1, did,tid; 
    unsigned long ul_rows, ul_cols, ul_nnz; 
    hid_t integer_type; 
    size_t integer_size, elemsize; 
    void *tmp; 

    /*-----------------------------------------------------------------------------
     *  Select Int type
     *-----------------------------------------------------------------------------*/
    if ( integer_field == CSC_HDF5_INTEGER8 ) {
        integer_type = H5T_NATIVE_CHAR;
        integer_size = sizeof(char); 
    } else if ( integer_field == CSC_HDF5_INTEGER16 ) {
        integer_type = H5T_NATIVE_SHORT; 
        integer_size = sizeof(short); 
    } else if ( integer_field == CSC_HDF5_INTEGER32 ) {
        integer_type = H5T_NATIVE_INT; 
        integer_size = sizeof(int); 
    } else if ( integer_field == CSC_HDF5_INTEGER64 ) {
        integer_type = H5T_NATIVE_LONG; 
        integer_size = sizeof(long); 
    } else {
        csc_error_message("Unsupported Integer type.\n"); 
        return -1; 
    }
    
    sparse_group = H5Gopen2(root,dset_name, H5P_DEFAULT);
    if (sparse_group < 0 ) {
        csc_error_message("Failed to open group %s\n",dset_name);  
        return -1; 
    }
   
    /*-----------------------------------------------------------------------------
     *  Read  attributes from the Group
     *-----------------------------------------------------------------------------*/
    if (csc_hdf5_get_content(root, dset_name, type) < 0 ) {
        csc_error_message("failed to read the content type\n"); 
        H5Gclose(sparse_group); 
        return -1; 
    }

    if ( *type != CSC_HDF5_SPARSE_COORDINATE 
         && *type != CSC_HDF5_SPARSE_CSC
         && *type != CSC_HDF5_SPARSE_CSR ) {
        csc_error_message("Content type not supported for sparse matrices.\n"); 
        H5Gclose(sparse_group); 
        return -1; 
    }

    if ( csc_hdf5_get_field(root, dset_name, number_field) < 0 ) {
        csc_error_message("failed to read the field type\n"); 
        H5Gclose(sparse_group); 
        return -1; 
    }


    HDF5_GET_ATTR_ULONG(root,dset_name,"rows",ul_rows); 
    HDF5_GET_ATTR_ULONG(root,dset_name,"cols",ul_cols); 
    HDF5_GET_ATTR_ULONG(root,dset_name,"nnz",ul_nnz); 
 
 
    *cols = ul_cols; 
    *rows = ul_rows; 
    *nnz = ul_nnz; 


    /*-----------------------------------------------------------------------------
     *  Allocate Temporary Storage 
     *-----------------------------------------------------------------------------*/
    if((did = H5Dopen2(sparse_group, "rowptr",H5P_DEFAULT)) < 0) {
        H5Gclose(sparse_group); 
        csc_error_message("Failed to open dataset %s\n", dset_name); 
        return -1; 
    }
    tid = H5Dget_type(did); 
    elemsize = H5Tget_size(tid); 
    elemsize = (elemsize > integer_size ) ? elemsize: integer_size; 
    tmp = malloc(elemsize * MAX(MAX(ul_nnz,ul_cols+1),ul_rows+1)); 
    if ( tmp == NULL ) {
        csc_error_message("failed to allocate temporary storage\n"); 
        H5Tclose(tid);  H5Dclose(did);  H5Gclose(sparse_group); 
        free(tmp);
        return -1; 
    }
    H5Dclose(did); 


    /*-----------------------------------------------------------------------------
     *  Read Data 
     *-----------------------------------------------------------------------------*/
    if ( *type == CSC_HDF5_SPARSE_COORDINATE ) {
        /* Rowptr  */
		err = H5LTread_dataset(sparse_group, "rowptr", tid, tmp); 
        if ( err < 0 ) {
            csc_error_message("failed to read rowptr\n"); 
            H5Tclose(tid);  H5Gclose(sparse_group); 
            free(tmp);
            return -1; 
        }

        if ( H5Tconvert(tid, integer_type, ul_nnz, tmp, NULL, H5P_DEFAULT)< 0) {
            csc_error_message("failed to convert values from %s/rowptr\n", dset_name); 
            H5Tclose(tid);  H5Gclose(sparse_group); 
            free(tmp); 
            return -1; 
        }
        memcpy(rowptr, tmp, integer_size * ul_nnz); 

        /* Colptr  */
        err = H5LTread_dataset(sparse_group, "colptr", tid, tmp); 
        if ( err < 0 ) {
            csc_error_message("failed to read colptr\n"); 
            H5Tclose(tid); H5Gclose(sparse_group); 
            free(tmp);
            return -1; 
        }

        if ( H5Tconvert(tid, integer_type, ul_nnz, tmp, NULL, H5P_DEFAULT)< 0) {
            csc_error_message("failed to convert values from %s/colptr\n", dset_name); 
            H5Tclose(tid); H5Gclose(sparse_group); 
            free(tmp); 
            return -1; 
        }
        memcpy(colptr, tmp, integer_size * ul_nnz); 

    } else if ( *type == CSC_HDF5_SPARSE_CSC ) {
        /* Rowptr  */
		err = H5LTread_dataset(sparse_group, "rowptr", tid, tmp); 
        if ( err < 0 ) {
            csc_error_message("failed to read rowptr\n"); 
            H5Tclose(tid);  H5Gclose(sparse_group); 
            free(tmp);
            return -1; 
        }

        if ( H5Tconvert(tid, integer_type, ul_nnz, tmp, NULL, H5P_DEFAULT)< 0) {
            csc_error_message("failed to convert values from %s/rowptr\n", dset_name); 
            H5Tclose(tid);  H5Gclose(sparse_group); 
            free(tmp); 
            return -1; 
        }
        memcpy(rowptr, tmp, integer_size * ul_nnz); 

        /* Colptr  */
        err = H5LTread_dataset(sparse_group, "colptr", tid, tmp); 
        if ( err < 0 ) {
            csc_error_message("failed to read colptr\n"); 
            H5Tclose(tid); H5Gclose(sparse_group); 
            free(tmp);
            return -1; 
        }

        if ( H5Tconvert(tid, integer_type, ul_cols+1, tmp, NULL, H5P_DEFAULT)< 0) {
            csc_error_message("failed to convert values from %s/colptr\n", dset_name); 
            H5Tclose(tid); H5Gclose(sparse_group); 
            free(tmp); 
            return -1; 
        }
        memcpy(colptr, tmp, integer_size * (ul_cols+1)); 
        
    } else if ( *type == CSC_HDF5_SPARSE_CSR ) {
         /* Rowptr  */
		err = H5LTread_dataset(sparse_group, "rowptr", tid, tmp); 
        if ( err < 0 ) {
            csc_error_message("failed to read rowptr\n"); 
            H5Tclose(tid);  H5Gclose(sparse_group); 
            free(tmp);
            return -1; 
        }

        if ( H5Tconvert(tid, integer_type, ul_rows+1, tmp, NULL, H5P_DEFAULT)< 0) {
            csc_error_message("failed to convert values from %s/rowptr\n", dset_name); 
            H5Tclose(tid);  H5Gclose(sparse_group); 
            free(tmp); 
            return -1; 
        }
        memcpy(rowptr, tmp, integer_size * (ul_rows+1)); 

        /* Colptr  */
        err = H5LTread_dataset(sparse_group, "colptr", tid, tmp); 
        if ( err < 0 ) {
            csc_error_message("failed to read colptr\n"); 
            H5Tclose(tid); H5Gclose(sparse_group); 
            free(tmp);
            return -1; 
        }

        if ( H5Tconvert(tid, integer_type, ul_nnz, tmp, NULL, H5P_DEFAULT)< 0) {
            csc_error_message("failed to convert values from %s/colptr\n", dset_name); 
            H5Tclose(tid); H5Gclose(sparse_group); 
            free(tmp); 
            return -1; 
        }
        memcpy(colptr, tmp, integer_size * (ul_nnz)); 
   }
   H5Tclose(tid); 
   free(tmp); 

   /* Read values  */
    if ( *number_field == CSC_HDF5_REAL ) {
    	err = H5LTread_dataset(sparse_group, "values", H5T_NATIVE_DOUBLE, values); 
    } else if ( *number_field == CSC_HDF5_COMPLEX ) {
        hid_t complex_type; 
        complex_type = csc_hdf5_complex_type(root); 
    	if ( complex_type < 0 ) {
	    	csc_error_message("Failed to create complex type.\n"); 
    		return 0; 
	    }
    	err = H5LTread_dataset(sparse_group, "values", complex_type, values); 
        H5Tclose(complex_type); 
    }

    if ( err < 0 ) {
		csc_error_message("Failed to set values data.\n"); 
        H5Gclose(sparse_group); 
        return -1; 
    }

    H5Gclose(sparse_group); 

    return 0; 
}


