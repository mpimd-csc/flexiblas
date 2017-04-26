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

/* Write a matrix to a data set   */
int csc_hdf5_matrix_write_real(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t ld, double *matrix) 
{
	hsize_t dims[2]; 
	herr_t err; 
    hid_t vg; 
    unsigned long ul_rows, ul_cols; 

	/* if (rows != ld) {
		csc_error_message("Leading dimension must be the same as the number of rows. \n"); 
		return -1; 
	} */

    if ( H5Lexists(root, dset_name, H5P_DEFAULT) > 0 ) {
        if ( H5Ldelete(root, dset_name, H5P_DEFAULT) < 0 ) {
            csc_error_message("Failed to remove previous entry %s\n", dset_name); 
            return -1; 
        }
    }
	/* Store rows and cols in transposed way to be able to use Fortran storage for the matrix  */
	vg = H5Gcreate2(root,dset_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT); 
    if (vg < 0 ) {
        csc_error_message("failed to create group for the data set."); 
        return -1; 
    }

    err = csc_hdf5_set_content(root,dset_name,CSC_HDF5_MATRIX);  
	if ( err < 0 ) { 
		csc_error_message("Failed to set the content type.\n");
        H5Gclose(vg); 
		return -1; 
	}
    err = csc_hdf5_set_field(root,dset_name,CSC_HDF5_REAL);  
	if ( err < 0 ) { 
		csc_error_message("Failed to set the field type.\n"); 
        H5Gclose(vg); 
		return -1; 
	}

    ul_cols = (unsigned long) cols; 
    ul_rows = (unsigned long) rows; 

    err = H5LTset_attribute_ulong(root, dset_name, "rows", &ul_rows, 1); 
	if ( err < 0 ) {
		csc_error_message("Failed to set attribute rows.\n"); 
        H5Gclose(vg); 
		return -1; 
	}
    
    err = H5LTset_attribute_ulong(root, dset_name, "cols", &ul_cols, 1); 
	if ( err < 0 ) {
		csc_error_message("Failed to set attribute cols.\n"); 
        H5Gclose(vg); 
		return -1; 
	}


	dims[0] = cols; 
	dims[1] = rows; 
	if ( rows != ld /* || (H5Tget_order(H5T_NATIVE_DOUBLE) != H5T_ORDER_LE) */ ) {
		hsize_t     count[2];              /*  size of subset in the file */
		hsize_t     offset[2];             /*  subset offset in the file */
		hsize_t     stride[2];
		hsize_t     blocks[2];
		hsize_t     dim_block[2];
		size_t      j;
		hid_t 	    memspace_id;
		hid_t  	    dataspace_id;
		hid_t       dataset_id;
	    	printf("hier\n");
		err = csc_hdf5_create_dataset(vg, "values", 2, dims, H5T_NATIVE_DOUBLE, NULL); 
		if ( err < 0 ) {
			csc_error_message("Failed to write a real matrix of size (%lu, %lu) to %s\n", (unsigned long) rows, (unsigned long) cols, dset_name); 
            H5Gclose(vg); 
			return -1; 
		}
		dataset_id = H5Dopen2 (vg, "values", H5P_DEFAULT);
		if ( dataset_id < 0 ) {
			csc_error_message("Failed to get the data space from %s.\n", dset_name);
			return -1;
		}
		dim_block[0] = 1;
		dim_block[1] = rows;

		for( j = 0; j < cols; j++) {
			count[0] = 1;
			count[1] = rows;

			offset[0] = j;
			offset[1] = 0;

			stride[0] = 1;
			stride[1] = 1;
			blocks[0] = 1;
			blocks[1] = 1;
		   	memspace_id = H5Screate_simple (2, dim_block, NULL); 
            dataspace_id = H5Dget_space (dataset_id);
			err  = H5Sselect_hyperslab (dataspace_id, H5S_SELECT_SET, offset, stride, count, blocks);
			if ( err < 0 )  {
				csc_error_message("Failed to select hyperslab.\n");
				H5Dclose(dataset_id);
                H5Sclose(memspace_id);
                H5Sclose(dataspace_id);
                H5Gclose(vg); 	
                return -1;
            }
            err  = H5Dwrite (dataset_id, H5T_NATIVE_DOUBLE, memspace_id, dataspace_id, H5P_DEFAULT, matrix+j*ld);
            if ( err < 0 )     {
                csc_error_message("Failed to write column %d\n", (int) j);
                H5Dclose(dataset_id);
                H5Sclose(memspace_id);
                H5Sclose(dataspace_id);
                H5Gclose(vg); 	
                return -1;
            }

            H5Sclose(memspace_id);
            H5Sclose(dataspace_id);
		}
		H5Dclose(dataset_id);
	} else {
		err = csc_hdf5_create_dataset(vg, "values", 2, dims, H5T_NATIVE_DOUBLE, matrix); 
		if ( err < 0 ) {
			csc_error_message("Failed to write a real matrix of size (%lu, %lu) to %s\n", (unsigned long) rows, (unsigned long) cols, dset_name); 
			return -1; 
		}
	}
    H5Gclose(vg); 
	return 0; 
}

/* Write a matrix to a data set   */
int csc_hdf5_matrix_write_real_single(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t ld, float *matrix) 
{
	hsize_t dims[2]; 
	herr_t err; 
    hid_t vg; 
    unsigned long ul_rows, ul_cols; 

	/* if (rows != ld) {
		csc_error_message("Leading dimension must be the same as the number of rows. \n"); 
		return -1; 
	} */

    if ( H5Lexists(root, dset_name, H5P_DEFAULT) > 0 ) {
        if ( H5Ldelete(root, dset_name, H5P_DEFAULT) < 0 ) {
            csc_error_message("Failed to remove previous entry %s\n", dset_name); 
            return -1; 
        }
    }
	/* Store rows and cols in transposed way to be able to use Fortran storage for the matrix  */
	vg = H5Gcreate2(root,dset_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT); 
    if (vg < 0 ) {
        csc_error_message("failed to create group for the data set."); 
        return -1; 
    }

    err = csc_hdf5_set_content(root,dset_name,CSC_HDF5_MATRIX);  
	if ( err < 0 ) { 
		csc_error_message("Failed to set the content type.\n");
        H5Gclose(vg); 
		return -1; 
	}
    err = csc_hdf5_set_field(root,dset_name,CSC_HDF5_REAL_SINGLE);  
	if ( err < 0 ) { 
		csc_error_message("Failed to set the field type.\n"); 
        H5Gclose(vg); 
		return -1; 
	}

    ul_cols = (unsigned long) cols; 
    ul_rows = (unsigned long) rows; 

    err = H5LTset_attribute_ulong(root, dset_name, "rows", &ul_rows, 1); 
	if ( err < 0 ) {
		csc_error_message("Failed to set attribute rows.\n"); 
        H5Gclose(vg); 
		return -1; 
	}
    
    err = H5LTset_attribute_ulong(root, dset_name, "cols", &ul_cols, 1); 
	if ( err < 0 ) {
		csc_error_message("Failed to set attribute cols.\n"); 
        H5Gclose(vg); 
		return -1; 
	}


	dims[0] = cols; 
	dims[1] = rows; 
	if ( rows != ld /* || (H5Tget_order(H5T_NATIVE_DOUBLE) != H5T_ORDER_LE) */ ) {
		hsize_t     count[2];              /*  size of subset in the file */
		hsize_t     offset[2];             /*  subset offset in the file */
		hsize_t     stride[2];
		hsize_t     blocks[2];
		hsize_t     dim_block[2];
		size_t      j;
		hid_t 	    memspace_id;
		hid_t  	    dataspace_id;
		hid_t       dataset_id;
		
        err = csc_hdf5_create_dataset(vg, "values", 2, dims, H5T_NATIVE_FLOAT, NULL); 
		if ( err < 0 ) {
			csc_error_message("Failed to write a real matrix of size (%lu, %lu) to %s\n", (unsigned long) rows, (unsigned long) cols, dset_name); 
            H5Gclose(vg); 
			return -1; 
		}
		dataset_id = H5Dopen2 (vg, "values", H5P_DEFAULT);
		if ( dataset_id < 0 ) {
			csc_error_message("Failed to get the data space from %s.\n", dset_name);
			return -1;
		}
		dim_block[0] = 1;
		dim_block[1] = rows;

		for( j = 0; j < cols; j++) {
			count[0] = 1;
			count[1] = rows;

			offset[0] = j;
			offset[1] = 0;

			stride[0] = 1;
			stride[1] = 1;
			blocks[0] = 1;
			blocks[1] = 1;
		   	memspace_id = H5Screate_simple (2, dim_block, NULL); 
            dataspace_id = H5Dget_space (dataset_id);
			err  = H5Sselect_hyperslab (dataspace_id, H5S_SELECT_SET, offset, stride, count, blocks);
			if ( err < 0 )  {
				csc_error_message("Failed to select hyperslab.\n");
				H5Dclose(dataset_id);
                H5Sclose(memspace_id);
                H5Sclose(dataspace_id);
                H5Gclose(vg); 	
                return -1;
            }
            err  = H5Dwrite (dataset_id, H5T_NATIVE_FLOAT, memspace_id, dataspace_id, H5P_DEFAULT, matrix+j*ld);
            if ( err < 0 )     {
                csc_error_message("Failed to write column %d\n", (int) j);
                H5Dclose(dataset_id);
                H5Sclose(memspace_id);
                H5Sclose(dataspace_id);
                H5Gclose(vg); 	
                return -1;
            }

            H5Sclose(memspace_id);
            H5Sclose(dataspace_id);
		}
		H5Dclose(dataset_id);
	} else {
		err = csc_hdf5_create_dataset(vg, "values", 2, dims, H5T_NATIVE_FLOAT, matrix); 
		if ( err < 0 ) {
			csc_error_message("Failed to write a real matrix of size (%lu, %lu) to %s\n", (unsigned long) rows, (unsigned long) cols, dset_name); 
			return -1; 
		}
	}
    H5Gclose(vg); 
	return 0; 
}

/* Write complex a matrix to a data set   */

int csc_hdf5_matrix_write_complex(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t ld, double complex *matrix) 
{
    hsize_t dims[2]; 
    herr_t err;  
    hid_t  complex_type;
    hid_t vg; 

    unsigned long ul_rows, ul_cols; 

    /*-----------------------------------------------------------------------------
     *  Basic Attributes 
     *-----------------------------------------------------------------------------*/

    if ( H5Lexists(root, dset_name, H5P_DEFAULT) > 0 ) {
        if ( H5Ldelete(root, dset_name, H5P_DEFAULT) < 0 ) {
            csc_error_message("Failed to remove previous entry %s\n", dset_name); 
            return -1; 
        }
    }
	vg = H5Gcreate2(root,dset_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT); 
    if (vg < 0 ) {
        csc_error_message("failed to create group for the data set."); 
        return -1; 
    }

    err = csc_hdf5_set_content(root,dset_name,CSC_HDF5_MATRIX);  
	if ( err < 0 ) { 
		csc_error_message("Failed to set the content type.\n");
        H5Gclose(vg); 
		return -1; 
	}
    err = csc_hdf5_set_field(root,dset_name,CSC_HDF5_COMPLEX);  
	if ( err < 0 ) { 
		csc_error_message("Failed to set the field type.\n"); 
        H5Gclose(vg); 
		return -1; 
	}

    ul_cols = (unsigned long) cols; 
    ul_rows = (unsigned long) rows; 

    err = H5LTset_attribute_ulong(root, dset_name, "rows", &ul_rows, 1); 
	if ( err < 0 ) {
		csc_error_message("Failed to set attribute rows.\n"); 
        H5Gclose(vg); 
		return -1; 
	}
    
    err = H5LTset_attribute_ulong(root, dset_name, "cols", &ul_cols, 1); 
	if ( err < 0 ) {
		csc_error_message("Failed to set attribute cols.\n"); 
        H5Gclose(vg); 
		return -1; 
	}


    /* Store rows and cols in transposed way to be able to use Fortran storage for the matrix  */
    dims[0] = cols; 
    dims[1] = rows; 

    complex_type = csc_hdf5_complex_type(root); 
    if ( complex_type < 0 ) {
        csc_error_message("Failed to create complex type.\n"); 
        return 0; 
    }

    if ( rows != ld ) {
        hsize_t     count[2];              /*  size of subset in the file */
        hsize_t     offset[2];             /*  subset offset in the file */
        hsize_t     stride[2];
        hsize_t     blocks[2];
        hsize_t     dim_block[2];
        size_t      j;
        hid_t 	    memspace_id;
        hid_t  	    dataspace_id;
        hid_t       dataset_id;

        err = csc_hdf5_create_dataset(vg, "values", 2, dims, complex_type, NULL); 
        if ( err < 0 ) {
            csc_error_message("Failed to write a real matrix of size (%lu, %lu) to %s\n", (unsigned long) rows, (unsigned long) cols, "values"); 
            H5Tclose(complex_type); 
            H5Gclose(vg); 
            return -1; 
        }
        dataset_id = H5Dopen2 (vg,"values", H5P_DEFAULT);
        if ( dataset_id < 0 ) {
            csc_error_message("Failed to get the data space from %s.\n", "values");
            H5Tclose(complex_type); 
            H5Gclose(vg); 
            return -1;
        }
        dim_block[0] = 1;
        dim_block[1] = rows;

        for( j = 0; j < cols; j++) {
            count[0] = 1;
            count[1] = rows;

            offset[0] = j;
            offset[1] = 0;

            stride[0] = 1;
            stride[1] = 1;
            blocks[0] = 1;
            blocks[1] = 1;
            memspace_id = H5Screate_simple (2, dim_block, NULL); 
            dataspace_id = H5Dget_space (dataset_id);
            err  = H5Sselect_hyperslab (dataspace_id, H5S_SELECT_SET, offset, stride, count, blocks);
            if ( err < 0 )  {
                csc_error_message("Failed to select hyperslab.\n");
                H5Dclose(dataset_id);
                H5Sclose(memspace_id);
                H5Sclose(dataspace_id);
                H5Tclose(complex_type); 
                H5Gclose(vg); 


                return -1;
            }
            err  = H5Dwrite (dataset_id, complex_type, memspace_id, dataspace_id, H5P_DEFAULT, matrix+j*ld);
            if ( err < 0 )     {
                csc_error_message("Failed to write column %d\n", (int) j);
                H5Dclose(dataset_id);
                H5Sclose(memspace_id);
                H5Sclose(dataspace_id);
                H5Tclose(complex_type); 
                H5Gclose(vg); 

                return -1;
            }
            H5Sclose(memspace_id);
            H5Sclose(dataspace_id);
        }
        H5Dclose(dataset_id);
    } else {
        err = csc_hdf5_create_dataset(vg, "values", 2, dims, complex_type, matrix); 
        if ( err < 0 ) {
            csc_error_message("Failed to write a complex matrix of size (%lu, %lu) to %s\n", (unsigned long) rows, (unsigned long) cols, "values"); 
            return -1; 
        }
    }



    H5Tclose(complex_type); 
    H5Gclose(vg); 

    return 0; 
}





/* Read the size of a matrix from  a data set   */
int csc_hdf5_matrix_size(hid_t root, const char *dset_name, size_t *rows, size_t *cols) 
{
	if (! csc_hdf5_is_matrix(root, dset_name)){
		csc_error_message("Dataset %s is not a matrix.\n", dset_name); 
		return -1; 
	}
    HDF5_GET_ATTR_ULONG(root,dset_name,"rows",*rows); 
    HDF5_GET_ATTR_ULONG(root,dset_name,"cols",*cols); 
	return 0; 
}


/* Write a matrix to a data set   */
int csc_hdf5_matrix_read_real(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t ld, double *matrix) 
{
    hsize_t dims[2]; 
    hsize_t dims_read[2]; 
    hsize_t rank; 
    herr_t err;  
    hid_t vg; 
    hid_t dataspace_id; 
    hid_t dataset_id; 
    hid_t dtype_id; 
    hid_t native_dtype_id; 
    size_t irows, icols; 


    /*-----------------------------------------------------------------------------
     *  Check
     *-----------------------------------------------------------------------------*/
    if (! csc_hdf5_is_matrix(root, dset_name) ) {
        csc_error_message("Selected DSET %s is not a Matrix.\n", dset_name); 
        return -1;  
    }

    if (!csc_hdf5_is_real(root, dset_name)) {
        csc_error_message("Selected Element %s is not a real.\n", dset_name); 
        return -1; 
    }

    HDF5_GET_ATTR_ULONG(root,dset_name,"cols",icols); 
    HDF5_GET_ATTR_ULONG(root,dset_name,"rows",irows); 
    if ( icols != cols || irows != rows) {
        csc_error_message("Requested matrix dimension does not fit with the one stored in the data set."); 
        return -1; 
    }


    /*-----------------------------------------------------------------------------
     *  Open Group 
     *-----------------------------------------------------------------------------*/
    vg = H5Gopen2(root, dset_name, H5P_DEFAULT); 
    if ( vg < 0) {
        csc_error_message("Failed to open data set %s.\n", dset_name); 
        return -1; 
    }

    /* Store rows and cols in transposed way to be able to use Fortran storage for the matrix  */
    dims[0] = cols; 
    dims[1] = rows; 

    dataset_id = H5Dopen2(vg, "values", H5P_DEFAULT); 
    if ( dataset_id < 0 ) {
        csc_error_message("Failed to open data set %s.\n", dset_name); 
        H5Gclose(vg); 
        return -1; 
    }

    dataspace_id = H5Dget_space(dataset_id); 
    if ( dataspace_id < 0) {
        csc_error_message("Failed to get the data space from %s.\n", dset_name); 
        H5Dclose(dataset_id);
        H5Gclose(vg); 

        return -1; 
    }

    if((rank = H5Sget_simple_extent_ndims(dataspace_id)) < 0) {
        csc_error_message("Failed to get the rank of the data set %s.\n", dset_name); 
        H5Dclose(dataspace_id); 
        H5Sclose(dataset_id); 
        H5Gclose(vg); 

        return -1; 
    }

    if ( rank != 2 ) {
        csc_error_message("Data set %s of rank %d is not a matrix.\n", dset_name, (int) rank); 
        H5Sclose(dataspace_id); 
        H5Dclose(dataset_id); 
        H5Gclose(vg); 

        return -1; 

    }

    if(H5Sget_simple_extent_dims(dataspace_id, dims_read, NULL) < 0) {
        csc_error_message("Failed to get the dimensions of the data set %s.\n", dset_name); 
        H5Sclose(dataspace_id); 
        H5Dclose(dataset_id); 
        H5Gclose(vg); 

        return -1; 

    }

    if ( dims_read[0] != dims[0] || dims_read[1] != dims[1]) {
        csc_error_message("The read dimensions (%d, %d) do not fit to the requested ones (%d, %d) (dset_name = %s)\n", 
                (int) dims_read[1], (int) dims_read[0], (int) dims[1], (int) dims[0], dset_name); 
        H5Sclose(dataspace_id); 
        H5Dclose(dataset_id); 
        H5Gclose(vg); 

        return -1; 
    }

    dtype_id = H5Dget_type(dataset_id); 
    if ( dtype_id < 0 ) {
        csc_error_message("Failed to get the data type from the data set %s\n", dset_name); 
        H5Sclose(dataspace_id); 
        H5Dclose(dataset_id); 
        H5Gclose(vg); 

        return -1; 
    }

    native_dtype_id = H5Tget_native_type(dtype_id, H5T_DIR_ASCEND); 
    if ( H5Tequal(native_dtype_id, H5T_NATIVE_DOUBLE) <= 0 ) {
        csc_error_message("The data type in the data set %s is not double precision.\n", dset_name); 
        H5Tclose(native_dtype_id); 
        H5Tclose(dtype_id); 
        H5Sclose(dataspace_id); 
        H5Dclose(dataset_id); 
        H5Gclose(vg); 

        return -1; 
    }

    if ( rows == ld ) {
        /* Read the data  */
        err = H5Dread(dataset_id, native_dtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, matrix); 
        if ( err < 0 ) {
            csc_error_message("Failed to read the data from %d\n", dset_name); 
            H5Tclose(native_dtype_id); 
            H5Tclose(dtype_id); 
            H5Sclose(dataspace_id); 
            H5Dclose(dataset_id); 
            H5Gclose(vg); 
            return -1;            
        }
    } else {
        size_t j; 
        hsize_t     count[2];              /*  size of subset in the file  */
        hsize_t     offset[2];             /*  subset offset in the file */ 
        hsize_t     stride[2];
        hsize_t     blocks[2];
        hsize_t     dim_block[2];
        hid_t 	    memspace_id;
        hid_t  	    dataspace_loc_id;

        dim_block[0] = 1;
        dim_block[1] = rows;

        for( j = 0; j < cols; j++) {
            count[0] = 1;
            count[1] = rows;

            offset[0] = j;
            offset[1] = 0;

            stride[0] = 1;
            stride[1] = 1;
            blocks[0] = 1;
            blocks[1] = 1;
            memspace_id = H5Screate_simple (2, dim_block, NULL); 
            dataspace_loc_id = H5Dget_space (dataset_id);
            err  = H5Sselect_hyperslab (dataspace_loc_id, H5S_SELECT_SET, offset, stride, count, blocks);
            if ( err < 0 )  {
                csc_error_message("Failed to select hyperslab.\n");
                H5Sclose(memspace_id);
                H5Sclose(dataspace_loc_id);
                H5Tclose(native_dtype_id); 
                H5Tclose(dtype_id); 
                H5Sclose(dataspace_id); 
                H5Dclose(dataset_id); 
                H5Gclose(vg); 

                return -1;
            }

            err  = H5Dread (dataset_id, native_dtype_id, memspace_id, dataspace_loc_id, H5P_DEFAULT, matrix+j*ld);
            if ( err < 0 )     {
                csc_error_message("Failed to read column %d\n", (int) j);
                H5Sclose(memspace_id);
                H5Sclose(dataspace_loc_id);
                H5Tclose(native_dtype_id); 
                H5Tclose(dtype_id); 
                H5Sclose(dataspace_id); 
                H5Dclose(dataset_id); 
                H5Gclose(vg); 
                return -1;
            }
            H5Sclose(memspace_id);
            H5Sclose(dataspace_loc_id);
        }
    }

    H5Tclose(native_dtype_id); 
    H5Tclose(dtype_id); 
    H5Sclose(dataspace_id); 
    H5Dclose(dataset_id); 
    H5Gclose(vg); 
    return 0; 
}

/* Write a matrix to a data set   */
int csc_hdf5_matrix_read_real_single(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t ld, float *matrix) 
{
    hsize_t dims[2]; 
    hsize_t dims_read[2]; 
    hsize_t rank; 
    herr_t err;  
    hid_t vg; 
    hid_t dataspace_id; 
    hid_t dataset_id; 
    hid_t dtype_id; 
    hid_t native_dtype_id; 
    size_t irows, icols; 


    /*-----------------------------------------------------------------------------
     *  Check
     *-----------------------------------------------------------------------------*/
    if (! csc_hdf5_is_matrix(root, dset_name) ) {
        csc_error_message("Selected DSET %s is not a Matrix.\n", dset_name); 
        return -1;  
    }

    if (!csc_hdf5_is_real_single(root, dset_name)) {
        csc_error_message("Selected Element %s is not a real single precision.\n", dset_name); 
        return -1; 
    }

    HDF5_GET_ATTR_ULONG(root,dset_name,"cols",icols); 
    HDF5_GET_ATTR_ULONG(root,dset_name,"rows",irows); 
    if ( icols != cols || irows != rows) {
        csc_error_message("Requested matrix dimension does not fit with the one stored in the data set."); 
        return -1; 
    }


    /*-----------------------------------------------------------------------------
     *  Open Group 
     *-----------------------------------------------------------------------------*/
    vg = H5Gopen2(root, dset_name, H5P_DEFAULT); 
    if ( vg < 0) {
        csc_error_message("Failed to open data set %s.\n", dset_name); 
        return -1; 
    }

    /* Store rows and cols in transposed way to be able to use Fortran storage for the matrix  */
    dims[0] = cols; 
    dims[1] = rows; 

    dataset_id = H5Dopen2(vg, "values", H5P_DEFAULT); 
    if ( dataset_id < 0 ) {
        csc_error_message("Failed to open data set %s.\n", dset_name); 
        H5Gclose(vg); 
        return -1; 
    }

    dataspace_id = H5Dget_space(dataset_id); 
    if ( dataspace_id < 0) {
        csc_error_message("Failed to get the data space from %s.\n", dset_name); 
        H5Dclose(dataset_id);
        H5Gclose(vg); 

        return -1; 
    }

    if((rank = H5Sget_simple_extent_ndims(dataspace_id)) < 0) {
        csc_error_message("Failed to get the rank of the data set %s.\n", dset_name); 
        H5Dclose(dataspace_id); 
        H5Sclose(dataset_id); 
        H5Gclose(vg); 

        return -1; 
    }

    if ( rank != 2 ) {
        csc_error_message("Data set %s of rank %d is not a matrix.\n", dset_name, (int) rank); 
        H5Sclose(dataspace_id); 
        H5Dclose(dataset_id); 
        H5Gclose(vg); 

        return -1; 

    }

    if(H5Sget_simple_extent_dims(dataspace_id, dims_read, NULL) < 0) {
        csc_error_message("Failed to get the dimensions of the data set %s.\n", dset_name); 
        H5Sclose(dataspace_id); 
        H5Dclose(dataset_id); 
        H5Gclose(vg); 

        return -1; 

    }

    if ( dims_read[0] != dims[0] || dims_read[1] != dims[1]) {
        csc_error_message("The read dimensions (%d, %d) do not fit to the requested ones (%d, %d) (dset_name = %s)\n", 
                (int) dims_read[1], (int) dims_read[0], (int) dims[1], (int) dims[0], dset_name); 
        H5Sclose(dataspace_id); 
        H5Dclose(dataset_id); 
        H5Gclose(vg); 

        return -1; 
    }

    dtype_id = H5Dget_type(dataset_id); 
    if ( dtype_id < 0 ) {
        csc_error_message("Failed to get the data type from the data set %s\n", dset_name); 
        H5Sclose(dataspace_id); 
        H5Dclose(dataset_id); 
        H5Gclose(vg); 

        return -1; 
    }

    native_dtype_id = H5Tget_native_type(dtype_id, H5T_DIR_ASCEND); 
    if ( H5Tequal(native_dtype_id, H5T_NATIVE_FLOAT) <= 0 ) {
        csc_error_message("The data type in the data set %s is not double precision.\n", dset_name); 
        H5Tclose(native_dtype_id); 
        H5Tclose(dtype_id); 
        H5Sclose(dataspace_id); 
        H5Dclose(dataset_id); 
        H5Gclose(vg); 

        return -1; 
    }

    if ( rows == ld ) {
        /* Read the data  */
        err = H5Dread(dataset_id, native_dtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, matrix); 
        if ( err < 0 ) {
            csc_error_message("Failed to read the data from %d\n", dset_name); 
            H5Tclose(native_dtype_id); 
            H5Tclose(dtype_id); 
            H5Sclose(dataspace_id); 
            H5Dclose(dataset_id); 
            H5Gclose(vg); 
            return -1;            
        }
    } else {
        size_t j; 
        hsize_t     count[2];              /*  size of subset in the file  */
        hsize_t     offset[2];             /*  subset offset in the file */ 
        hsize_t     stride[2];
        hsize_t     blocks[2];
        hsize_t     dim_block[2];
        hid_t 	    memspace_id;
        hid_t  	    dataspace_loc_id;

        dim_block[0] = 1;
        dim_block[1] = rows;

        for( j = 0; j < cols; j++) {
            count[0] = 1;
            count[1] = rows;

            offset[0] = j;
            offset[1] = 0;

            stride[0] = 1;
            stride[1] = 1;
            blocks[0] = 1;
            blocks[1] = 1;
            memspace_id = H5Screate_simple (2, dim_block, NULL); 
            dataspace_loc_id = H5Dget_space (dataset_id);
            err  = H5Sselect_hyperslab (dataspace_loc_id, H5S_SELECT_SET, offset, stride, count, blocks);
            if ( err < 0 )  {
                csc_error_message("Failed to select hyperslab.\n");
                H5Sclose(memspace_id);
                H5Sclose(dataspace_loc_id);
                H5Tclose(native_dtype_id); 
                H5Tclose(dtype_id); 
                H5Sclose(dataspace_id); 
                H5Dclose(dataset_id); 
                H5Gclose(vg); 

                return -1;
            }

            err  = H5Dread (dataset_id, native_dtype_id, memspace_id, dataspace_loc_id, H5P_DEFAULT, matrix+j*ld);
            if ( err < 0 )     {
                csc_error_message("Failed to read column %d\n", (int) j);
                H5Sclose(memspace_id);
                H5Sclose(dataspace_loc_id);
                H5Tclose(native_dtype_id); 
                H5Tclose(dtype_id); 
                H5Sclose(dataspace_id); 
                H5Dclose(dataset_id); 
                H5Gclose(vg); 
                return -1;
            }
            H5Sclose(memspace_id);
            H5Sclose(dataspace_loc_id);
        }
    }

    H5Tclose(native_dtype_id); 
    H5Tclose(dtype_id); 
    H5Sclose(dataspace_id); 
    H5Dclose(dataset_id); 
    H5Gclose(vg); 
    return 0; 
}

/* Write a matrix to a data set   */
int csc_hdf5_matrix_read_complex(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t ld, double complex *matrix) 
{
    hsize_t dims[2]; 
    hsize_t dims_read[2]; 
    hsize_t rank; 
    herr_t err;  
    hid_t dataspace_id; 
    hid_t dataset_id; 
    hid_t dtype_id; 
    hid_t complex_type; 
    hid_t vg; 
    size_t icols,irows; 

    /*-----------------------------------------------------------------------------
     *  Check
     *-----------------------------------------------------------------------------*/
    if (! csc_hdf5_is_matrix(root, dset_name) ) {
        return -1; 
    }
    if (!csc_hdf5_is_complex(root, dset_name)) {
        return -1; 
    }

    HDF5_GET_ATTR_ULONG(root,dset_name,"cols",icols); 
    HDF5_GET_ATTR_ULONG(root,dset_name,"rows",irows); 
    if ( icols != cols || irows != rows) {
        csc_error_message("Requested matrix dimension does not fit with the one stored in the data set."); 
        return -1; 
    }

    
    
    complex_type = csc_hdf5_complex_type(root); 
	if ( complex_type < 0 ) {
		csc_error_message("Failed to create complex type.\n"); 
		return 0; 
	}


    /*-----------------------------------------------------------------------------
     *  Open Group 
     *-----------------------------------------------------------------------------*/
    vg = H5Gopen2(root, dset_name, H5P_DEFAULT); 
    if ( vg < 0) {
        csc_error_message("Failed to open data set %s.\n", dset_name); 
        return -1; 
    }


    /* Store rows and cols in transposed way to be able to use Fortran storage for the matrix  */
    dims[0] = cols; 
    dims[1] = rows; 

    dataset_id = H5Dopen2(vg, "values", H5P_DEFAULT); 
    if ( dataset_id < 0 ) {
        csc_error_message("Failed to open data set %s.\n", dset_name); 
        H5Tclose(complex_type); 
        return -1; 
    }

    dataspace_id = H5Dget_space(dataset_id); 
    if ( dataspace_id < 0) {
        csc_error_message("Failed to get the data space from %s.\n", dset_name); 
        H5Dclose(dataset_id); 
        H5Tclose(complex_type); 

        return -1; 
    }

    if((rank = H5Sget_simple_extent_ndims(dataspace_id)) < 0) {
        csc_error_message("Failed to get the rank of the data set %s.\n", dset_name); 
        H5Dclose(dataspace_id); 
        H5Sclose(dataset_id); 
        H5Tclose(complex_type); 

        return -1; 
    }

    if ( rank != 2 ) {
        csc_error_message("Data set %s of rank %d is not a matrix.\n", dset_name, (int) rank); 
        H5Sclose(dataspace_id); 
        H5Dclose(dataset_id);
        H5Tclose(complex_type); 

        return -1; 

    }

    if(H5Sget_simple_extent_dims(dataspace_id, dims_read, NULL) < 0) {
        csc_error_message("Failed to get the dimensions of the data set %s.\n", dset_name); 
        H5Sclose(dataspace_id); 
        H5Dclose(dataset_id); 
        H5Tclose(complex_type); 
        return -1; 

    }

    if ( dims_read[0] != dims[0] || dims_read[1] != dims[1]) {
        csc_error_message("The read dimensions (%d, %d) do not fit to the requested ones (%d, %d) (dset_name = %s)\n", 
                (int) dims_read[1], (int) dims_read[0], (int) dims[1], (int) dims[0], dset_name); 
        H5Sclose(dataspace_id); 
        H5Dclose(dataset_id); 
        H5Tclose(complex_type); 

        return -1; 
    }

    dtype_id = H5Dget_type(dataset_id); 
    if ( dtype_id < 0 ) {
        csc_error_message("Failed to get the data type from the data set %s\n", dset_name); 
        H5Sclose(dataspace_id); 
        H5Dclose(dataset_id); 
        H5Tclose(complex_type); 

        return -1; 
    }

    if (H5Tequal(dtype_id, complex_type) <= 0 ) {
        csc_error_message("The data type in the data set %s is not complex double precision.\n", dset_name); 
        H5Tclose(dtype_id); 
        H5Sclose(dataspace_id); 
        H5Dclose(dataset_id); 
        H5Tclose(complex_type); 
        return -1; 
    }

    if ( rows == ld ) {
        /* Read the data  */
        err = H5Dread(dataset_id, complex_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, matrix); 
        if ( err < 0 ) {
            csc_error_message("Failed to read the data from %d\n", dset_name); 
            H5Tclose(dtype_id); 
            H5Sclose(dataspace_id); 
            H5Dclose(dataset_id); 
            H5Tclose(complex_type); 
            return -1;            
        }
    } else {
        size_t j; 
        hsize_t     count[2];              /*  size of subset in the file  */
        hsize_t     offset[2];             /*  subset offset in the file */ 
        hsize_t     stride[2];
        hsize_t     blocks[2];
        hsize_t     dim_block[2];
        hid_t 	    memspace_id;
        hid_t  	    dataspace_loc_id;

        dim_block[0] = 1;
        dim_block[1] = rows;

        for( j = 0; j < cols; j++) {
            count[0] = 1;
            count[1] = rows;

            offset[0] = j;
            offset[1] = 0;

            stride[0] = 1;
            stride[1] = 1;
            blocks[0] = 1;
            blocks[1] = 1;
            memspace_id = H5Screate_simple (2, dim_block, NULL); 
            dataspace_loc_id = H5Dget_space (dataset_id);
            err  = H5Sselect_hyperslab (dataspace_loc_id, H5S_SELECT_SET, offset, stride, count, blocks);
            if ( err < 0 )  {
                csc_error_message("Failed to select hyperslab.\n");
                H5Sclose(memspace_id);
                H5Sclose(dataspace_loc_id);
                H5Tclose(dtype_id); 
                H5Sclose(dataspace_id); 
                H5Dclose(dataset_id); 
                H5Tclose(complex_type); 
                return -1;
            }

            err  = H5Dread (dataset_id, complex_type, memspace_id, dataspace_loc_id, H5P_DEFAULT, matrix+j*ld);
            if ( err < 0 )     {
                csc_error_message("Failed to read column %d\n", (int) j);
                H5Sclose(memspace_id);
                H5Sclose(dataspace_loc_id);
                H5Tclose(dtype_id); 
                H5Sclose(dataspace_id); 
                H5Dclose(dataset_id); 
                H5Tclose(complex_type); 
                return -1;
            }
            H5Sclose(memspace_id);
            H5Sclose(dataspace_loc_id);
        }
    }

    H5Tclose(dtype_id); 
    H5Sclose(dataspace_id); 
    H5Dclose(dataset_id); 
    H5Tclose(complex_type); 
    H5Gclose(vg); 
    return 0; 
}



