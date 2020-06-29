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

#include "cscutils/strutils.h"
#include "cscutils/hdf.h"
#include "csc_hdf5_common.h"

#include "filters/filter_bzip2.h"
#include "filters/filter_xz.h"
#include "filters/filter_zstd.h"

#define MAX(A,B) ((A)<(B)?(B):(A))
#define MIN(A,B) ((A)>(B)?(B):(A))




static int csc_hdf5_write_matrix_ex(hid_t root, const char *dset_name, hid_t dtype, size_t rows, size_t cols, void * matrix, size_t ld, int cpx_stride, csc_hdf5_options_t *opts)
{
    int ret = 0;
    herr_t err;

    hid_t memspace = 0;
    hid_t filespace = 0;
    hid_t dataset = 0;
    hid_t fapl = H5P_DEFAULT;
    hsize_t     count[2];              /*  size of subset in the file */
    hsize_t     offset[2];             /*  subset offset in the file */
    hsize_t     stride[2];
    hsize_t     blocks[2];
    hsize_t     dims[2];
    hid_t dtype_local = 0;
    csc_hdf5_options_t default_opts = CSC_HDF5_OPTIONS_INIT;


    if ( opts == NULL) {
        opts = &default_opts;
    }
    if ( opts->compress != CSC_HDF5_COMPRESS_NONE  && opts->chunksize == 0 ) {
        csc_error_message("enabled compression needs a chunksize > 0 \n");
        return -1;
    }

    if ( csc_hdf5_group_path_create(root, dset_name, 0)) {
        csc_error_message("Failed to create path -- %s.\n", dset_name);
        ret = -1;
        goto end;
    }

    /* Remove existing */
    if ( H5Lexists(root, dset_name, H5P_DEFAULT) > 0 ) {
        if ( H5Ldelete(root, dset_name, H5P_DEFAULT) < 0 ) {
            csc_error_message("Failed to remove previous entry %s\n", dset_name);
            ret = -1;
            goto end;
        }
    }


    /* Create the memory space */
    dims[0] = cols;
    if ( cpx_stride > 1 && opts->complex_as_compound ) {
        dims[1] = ld;
    } else {
        dims[1] = ld * cpx_stride  ;
    }
    memspace = H5Screate_simple(2, dims, NULL);
    if ( memspace < 0 ) {
        csc_error_message("Failed to create memspace.\n");
        ret = -1;
        goto end;
    }

    if ( cpx_stride > 1 && opts->complex_as_compound ) {
        dims[1] = rows;
    } else {
        dims[1] = rows * cpx_stride  ;
    }
    filespace = H5Screate_simple(2, dims, NULL);
    if ( filespace < 0 ) {
        csc_error_message("Failed to create filespace.\n");
        ret = -1;
        goto end;
    }

    /* Hyperslab for the memspace   */
    offset[0] = 0;
    offset[1] = 0;
    stride[0] = 1;
    stride[1] = 1;
    count[0] = cols;
    if ( cpx_stride > 1 && opts->complex_as_compound) {
        count[1] = rows;
    } else {
        count[1] = rows*cpx_stride;
    }
    blocks[0] = 1;
    blocks[1] = 1;

    err = H5Sselect_hyperslab(memspace, H5S_SELECT_SET, offset, stride, count, blocks);
    if ( err < 0 ) {
        csc_error_message("Failed to select hyperslab.\n");
        ret = -1;
        goto end;
    }

    fapl = H5Pcreate(H5P_DATASET_CREATE);
    if ( fapl < 0 ) {
        csc_error_message("Failed to create dataset access property.\n");
        ret = -1;
        goto end;
    }

    /*  Enable compression. */
    if (opts->compress != CSC_HDF5_COMPRESS_NONE) {
        if ( !csc_hdf5_has_compression(opts->compress)) {
            csc_error_message("Compression not available.\n");
            ret = -1;
            goto end;
        }
        dims[0] = MIN(cols,opts->chunksize);
        dims[1] = MIN(rows,opts->chunksize);
        err = H5Pset_chunk(fapl, 2, dims);
        if ( err < 0 ) {
            csc_error_message("failed to set chunk size\n");
            ret = 1;
            goto end;
        }
        if ( opts->compress == CSC_HDF5_COMPRESS_DEFLATE) {
            if ( H5Pset_deflate(fapl, opts->deflate_level) < 0 ) {
                csc_error_message("Failed to set zlib compression\n");
                ret = -1;
                goto end;
            }
        } else if ( opts->compress == CSC_HDF5_COMPRESS_BZIP2 ) {
            err = H5Pset_filter (fapl, (H5Z_filter_t)H5Z_FILTER_BZIP2, H5Z_FLAG_MANDATORY, (size_t)0, NULL);
            if (err < 0 ) {
                ret = -1;
                csc_error_message("Failed to set bzip2 compression.\n");
                goto end;
            }
        } else if ( opts->compress == CSC_HDF5_COMPRESS_XZ ) {
            err = H5Pset_filter (fapl, (H5Z_filter_t)H5Z_FILTER_XZ, H5Z_FLAG_MANDATORY, (size_t)0, NULL);
            if (err < 0 ) {
                ret = -1;
                csc_error_message("Failed to set xz compression.\n");
                goto end;
            }
        } else if ( opts->compress == CSC_HDF5_COMPRESS_ZSTD ) {
            err = H5Pset_filter (fapl, (H5Z_filter_t)H5Z_FILTER_ZSTD, H5Z_FLAG_MANDATORY, (size_t)0, NULL);
            if (err < 0 ) {
                ret = -1;
                csc_error_message("Failed to set zstd compression.\n");
                goto end;
            }
        }

    }

    /* Create the dataset  */
    if ( cpx_stride > 1 && opts->complex_as_compound) {
        size_t t = H5Tget_size(dtype);
        dtype_local = H5Tcreate(H5T_COMPOUND, 2*t);
        if ( dtype_local < 0 ) {
            csc_error_message("Failed to create complex type");
            ret = -1;
            goto end;
        }
        if (H5Tinsert(dtype_local, "real", 0, dtype) < 0) {
            ret = -1;
            goto end;
        }
        if (H5Tinsert(dtype_local, "imag", t, dtype) < 0) {
            ret = -1;
            goto end;
        }
        if (H5Tpack(dtype_local) < 0 ) {
            ret = -1;
            goto end;
        }
    } else {
        dtype_local = dtype;
    }

    dataset = H5Dcreate2(root, dset_name, dtype_local, filespace, H5P_DEFAULT, fapl, H5P_DEFAULT);
    if ( dataset < 0 ) {
        csc_error_message("Failed to create dataset %s\n", dset_name);
        ret = -1;
        goto end;
    }
    err = H5Dwrite(dataset, dtype_local, memspace, filespace, H5P_DEFAULT, matrix);
    if ( err < 0 ) {
        csc_error_message("Failed to write to %s\n", dset_name);
        ret = -1;
        goto end;
    }


    if ( csc_hdf5_attribute_write_int(root, dset_name, "ROWS", rows)) {
        csc_error_message("Failed to write attribute to %s\n", dset_name);
        ret = -1;
        goto end;
    }

    if ( csc_hdf5_attribute_write_int(root, dset_name, "COLS", cols)) {
        csc_error_message("Failed to write attribute to %s\n", dset_name);
        ret = -1;
        goto end;
    }
    if ( csc_hdf5_attribute_write_int(root, dset_name, "COMPLEX_STRIDE", cpx_stride)) {
        csc_error_message("Failed to write attribute to %s\n", dset_name);
        ret = -1;
        goto end;
    }
    if ( csc_hdf5_attribute_write_int(root, dset_name, "COMPLEX", (cpx_stride==2)?1:0)) {
        csc_error_message("Failed to write attribute to %s\n", dset_name);
        ret = -1;
        goto end;
    }

    /*  Add Matlab_CLASS  */
    if ( opts->matlab_attributes) {
        if ( dtype == H5T_NATIVE_FLOAT ) {
            if ( csc_hdf5_attribute_write_string(root, dset_name, "MATLAB_class", "single") ){
                ret = -1;
                goto end;
            }
        }
        if ( dtype == H5T_NATIVE_DOUBLE ) {
            if ( csc_hdf5_attribute_write_string(root, dset_name, "MATLAB_class", "double") ){
                ret = -1;
                goto end;
            }
        }
    }
    ret = 0;

end:
    if ( memspace > 0) H5Sclose(memspace);
    if ( filespace > 0 ) H5Sclose(filespace);
    if ( fapl > 0 && fapl != H5P_DEFAULT) H5Pclose(fapl);
    if ( dataset > 0 ) H5Dclose(dataset);
    if ( dtype_local != dtype ) H5Tclose(dtype_local);
    return ret;
}

static int csc_hdf5_write_matrix_ex1(hid_t root, const char *dset_name, hid_t dtype, size_t rows, size_t cols, void * matrix, size_t ld, int cpx_stride, csc_hdf5_options_t *opts)
{
    csc_hdf5_options_t default_opts = CSC_HDF5_OPTIONS_INIT;
    int ret = -1;
    char* tmp = NULL;
    char* tmp2 = NULL;
    size_t len = 0;
    size_t len2 = 0;

    if ( opts == NULL) {
        opts = &default_opts;
    }

    if ( opts->octave_style ) {
        len = strlen(dset_name);
        tmp = calloc(sizeof(char), len+20);
        tmp2 = calloc(sizeof(char), len+20);
        if ( ! tmp || !tmp2) {
            ret = -1;
            goto end;
        }
        size_t tmp_size = len + 20;
        strcpy(tmp, dset_name);
        csc_strremovedup(tmp, '/');
        len2 = strlen(tmp);
        if ( tmp[len2-1] == '/') {
            snprintf(tmp2, len2+20, "%svalue", tmp);
        } else {
            snprintf(tmp2, len2+20, "%s/value", tmp);
        }


        ret = csc_hdf5_write_matrix_ex(root, tmp2, dtype, rows, cols, (void *) matrix, ld, cpx_stride, opts);
        if ( ret != 0 ) {
            ret = -1;
            goto end;
        }

        ret = csc_hdf5_attribute_write_uchar(root, tmp, "OCTAVE_NEW_FORMAT", 1);
        if ( ret != 0 ) {
            ret = -1;
            goto end;
        }

        if ( tmp[len2-1] == '/') {
            snprintf(tmp2, len+20, "%stype", tmp);
        } else {
            snprintf(tmp2, len+20, "%s/type", tmp);
        }

        if ( dtype == H5T_NATIVE_FLOAT && cpx_stride == 1) {
            ret = csc_hdf5_write_string(root, tmp2, "float matrix");
        } else if (  dtype == H5T_NATIVE_FLOAT && cpx_stride == 2) {
            ret = csc_hdf5_write_string(root, tmp2 , "float complex matrix");
        } else if (  dtype == H5T_NATIVE_DOUBLE && cpx_stride == 1) {
            ret = csc_hdf5_write_string(root, tmp2, "matrix");
        } else if (  dtype == H5T_NATIVE_DOUBLE && cpx_stride == 2) {
            ret = csc_hdf5_write_string(root, tmp2, "complex matrix");
        }
        if ( ret != 0 ) {
            ret = -1;
            goto end;
        }


        ret = 0;
    } else {
        ret = csc_hdf5_write_matrix_ex(root, dset_name, dtype, rows, cols, (void *) matrix, ld, cpx_stride, opts);
    }

end:
    if ( tmp ) free(tmp);
    if ( tmp2 ) free(tmp2);
    return ret;

}

#define CHECK_INPUT \
    do {\
        if ( root < 0 ) {\
            return -1;\
        }\
        if ( dset_name == NULL ) {\
            return -2;\
        }\
        if ( matrix == NULL ) {\
            return -5;\
        }\
        if ( lda < MAX(1, rows)) {\
            return -6;\
        }\
    } while (0)

#define CHECK_INPUT_V \
    do {\
        if ( root < 0 ) {\
            return -1;\
        }\
        if ( dset_name == NULL ) {\
            return -2;\
        }\
        if ( matrix == NULL ) {\
            return -5;\
        }\
    } while (0)

#define FORTRAN_COMPRESS \
    csc_hdf5_options_t opts = CSC_HDF5_OPTIONS_INIT; \
    if (compress > 0 && compress < 10 ) {\
        opts.compress = CSC_HDF5_COMPRESS_DEFLATE; \
        opts.deflate_level = compress; \
        opts.chunksize = 1536; \
    }



/* C routines   */
int csc_hdf5_write_double_matrix( hid_t root, const char* dset_name, size_t rows, size_t cols, double * matrix, size_t lda , csc_hdf5_options_t * opts)
{
    CHECK_INPUT;
    return csc_hdf5_write_matrix_ex1(root, dset_name, H5T_NATIVE_DOUBLE, rows, cols, (void *) matrix, lda, 1, opts);

}

int csc_hdf5_write_float_matrix( hid_t root, const char* dset_name, size_t rows, size_t cols, float * matrix, size_t lda , csc_hdf5_options_t * opts)
{
    CHECK_INPUT;
    return csc_hdf5_write_matrix_ex(root, dset_name, H5T_NATIVE_FLOAT, rows, cols, (void *) matrix, lda, 1, opts);
}

int csc_hdf5_write_double_complex_matrix( hid_t root, const char* dset_name, size_t rows, size_t cols, double complex * matrix, size_t lda , csc_hdf5_options_t * opts)
{
    CHECK_INPUT;
    return csc_hdf5_write_matrix_ex(root, dset_name, H5T_NATIVE_DOUBLE, rows, cols, (void *) matrix, lda, 2, opts);
}

int csc_hdf5_write_float_complex_matrix( hid_t root, const char* dset_name, size_t rows, size_t cols, float complex * matrix, size_t lda , csc_hdf5_options_t * opts)
{
    CHECK_INPUT;
    return csc_hdf5_write_matrix_ex(root, dset_name, H5T_NATIVE_FLOAT, rows, cols, (void *) matrix, lda, 2, opts);
}

/* Vector  */
int csc_hdf5_write_double_vector( hid_t root, const char* dset_name, size_t rows, double * matrix, csc_hdf5_options_t * opts)
{
    CHECK_INPUT_V;
    return csc_hdf5_write_matrix_ex1(root, dset_name, H5T_NATIVE_DOUBLE, rows, 1, (void *) matrix, rows, 1, opts);

}

int csc_hdf5_write_float_vector( hid_t root, const char* dset_name, size_t rows, float * matrix, csc_hdf5_options_t * opts)
{
    CHECK_INPUT_V;
    return csc_hdf5_write_matrix_ex(root, dset_name, H5T_NATIVE_FLOAT, rows, 1, (void *) matrix, rows, 1, opts);
}

int csc_hdf5_write_double_complex_vector( hid_t root, const char* dset_name, size_t rows, double complex * matrix, csc_hdf5_options_t * opts)
{
    CHECK_INPUT_V;
    return csc_hdf5_write_matrix_ex(root, dset_name, H5T_NATIVE_DOUBLE, rows, 1, (void *) matrix, rows, 2, opts);
}

int csc_hdf5_write_float_complex_vector( hid_t root, const char* dset_name, size_t rows, float complex * matrix, csc_hdf5_options_t * opts)
{
    CHECK_INPUT_V;
    return csc_hdf5_write_matrix_ex(root, dset_name, H5T_NATIVE_FLOAT, rows, 1, (void *) matrix, rows, 2, opts);
}


/* Fortran Routines  */
int csc_hdf5_write_double_matrix_f( hid_t root, const char* dset_name, size_t rows, size_t cols, double * matrix, size_t lda, int compress)
{
    CHECK_INPUT;
    FORTRAN_COMPRESS;
    return csc_hdf5_write_matrix_ex1(root, dset_name, H5T_NATIVE_DOUBLE, rows, cols, (void *) matrix, lda, 1, &opts);
}

int csc_hdf5_write_float_matrix_f( hid_t root, const char* dset_name, size_t rows, size_t cols, float * matrix, size_t lda, int compress)
{
    CHECK_INPUT;
    FORTRAN_COMPRESS;
    return csc_hdf5_write_matrix_ex1(root, dset_name, H5T_NATIVE_FLOAT, rows, cols, (void *) matrix, lda, 1, &opts);
}

int csc_hdf5_write_double_complex_matrix_f( hid_t root, const char* dset_name, size_t rows, size_t cols, double complex * matrix, size_t lda, int compress)
{
    CHECK_INPUT;
    FORTRAN_COMPRESS;
    return csc_hdf5_write_matrix_ex1(root, dset_name, H5T_NATIVE_DOUBLE, rows, cols, (void *) matrix, lda, 2, &opts);
}

int csc_hdf5_write_float_complex_matrix_f( hid_t root, const char* dset_name, size_t rows, size_t cols, float complex * matrix, size_t lda, int compress)
{
    CHECK_INPUT;
    FORTRAN_COMPRESS;
    return csc_hdf5_write_matrix_ex1(root, dset_name, H5T_NATIVE_FLOAT, rows, cols, (void *) matrix, lda, 2, &opts);
}

/* Vector  */
int csc_hdf5_write_double_vector_f( hid_t root, const char* dset_name, size_t rows, double * matrix, int compress)
{
    CHECK_INPUT_V;
    FORTRAN_COMPRESS;
    return csc_hdf5_write_matrix_ex1(root, dset_name, H5T_NATIVE_DOUBLE, rows, 1, (void *) matrix, rows, 1, &opts);
}

int csc_hdf5_write_float_vector_f( hid_t root, const char* dset_name, size_t rows, float * matrix, int compress)
{
    CHECK_INPUT_V;
    FORTRAN_COMPRESS;
    return csc_hdf5_write_matrix_ex1(root, dset_name, H5T_NATIVE_FLOAT, rows, 1, (void *) matrix, rows, 1, &opts);
}

int csc_hdf5_write_double_complex_vector_f( hid_t root, const char* dset_name, size_t rows, double complex * matrix, int compress)
{
    CHECK_INPUT_V;
    FORTRAN_COMPRESS;
    return csc_hdf5_write_matrix_ex1(root, dset_name, H5T_NATIVE_DOUBLE, rows, 1, (void *) matrix, rows, 2, &opts);
}

int csc_hdf5_write_float_complex_vector_f( hid_t root, const char* dset_name, size_t rows, float complex * matrix, int compress)
{
    CHECK_INPUT_V;
    FORTRAN_COMPRESS;
    return csc_hdf5_write_matrix_ex1(root, dset_name, H5T_NATIVE_FLOAT, rows, 1, (void *) matrix, rows, 2, &opts);
}




static int csc_hdf5_read_matrix_ex(hid_t root, const char *dset_name, hid_t dtype, size_t rows, size_t cols, void * matrix, size_t ld, int cpx_stride, csc_hdf5_options_t *opts)
{
    int ret = -1;
    hsize_t     dims[2];
    hsize_t     read_dims[2];
    hsize_t     count[2];              /*  size of subset in the file */
    hsize_t     offset[2];             /*  subset offset in the file */
    hsize_t     stride[2];
    hsize_t     blocks[2];

    hid_t memspace = 0;
    hid_t filespace = 0;
    hid_t dataset = 0;
    hid_t datatype = 0;
    hid_t datatype_local = 0;
    herr_t err;
    int rank = 0;
    csc_hdf5_field_t field;
    int complex_as_compound = 0;
    char *tmp = NULL;

    datatype_local = dtype;

    if ( csc_hdf5_dataset_exist(root, dset_name)) {
        /*  New and MATLAB */
        dataset = H5Dopen2(root, dset_name, H5P_DEFAULT);
        if ( dataset < 0) {
            ret = -1;
            csc_error_message("Failed to open dataset %s\n", dset_name);
            goto end;
        }

        datatype = H5Dget_type(dataset);
        if ( datatype < 0 ) {
            csc_error_message("Could not get data type from %s\n", dset_name);
            ret = -1;
            goto end;
        }

        filespace = H5Dget_space(dataset);
        if ( filespace < 0 ) {
            csc_error_message("Failed to get data space from %s\n", dset_name);
            ret = -1;
            goto end;
        }

        rank = H5Sget_simple_extent_ndims(filespace);
        if ( rank < 0 ) {
            ret = -1;
            goto end;
        }
        if ( rank > 2 || rank <= 0 ) {
            ret = -1;
            goto end;
        }


        err = H5Sget_simple_extent_dims(filespace, read_dims, NULL);
        if ( err < 0 ) {
            csc_error_message("Failed to get dimension from %s\n");
            ret = -1;
            goto end;
        }

        if ( rank == 1) {
            ret = -11;
            csc_error_message("Reading onedimensional objects into two dimensional ones is not supported yet.\n");
            goto end;
        }

        if ( cols > read_dims[0] || rows > read_dims[1]) {
            csc_error_message("The read buffer (%lu , %lu) is too small for the data (%lu , %lu) \n", rows, cols, read_dims[1], read_dims[0]);
            ret = -1;
            goto end;
        }

        /*  check for complex  */
        if ( csc_hdf5_matrix_get_datatype(root, dset_name, &field) ) {
            ret = -1;
            csc_error_message("Failed to get data type from matrix.\n");
            goto end;
        }
        if ( field == CSC_HDF5_DOUBLE_COMPLEX || field == CSC_HDF5_FLOAT_COMPLEX) {
            if ( H5Tget_class(datatype) == H5T_COMPOUND) {
                complex_as_compound = 1;
                cpx_stride = 1;
            } else {
                complex_as_compound = 0;
                cpx_stride = 2;
            }
        }

        /* Create the memory space */
        dims[0] = cols;
        if ( complex_as_compound  ) {
            dims[1] = ld;
        } else {
            dims[1] = ld * cpx_stride  ;
        }
        memspace = H5Screate_simple(2, dims, NULL);
        if ( memspace < 0 ) {
            csc_error_message("Failed to create memspace.\n");
            ret = -1;
            goto end;
        }

        /* And its hyperslab selection */
        /* Hyperslab for the memspace   */
        offset[0] = 0;
        offset[1] = 0;
        stride[0] = 1;
        stride[1] = 1;
        count[0] = cols;
        if ( complex_as_compound ) {
            count[1] = rows;
        } else {
            count[1] = rows*cpx_stride;
        }
        blocks[0] = 1;
        blocks[1] = 1;
        err = H5Sselect_hyperslab(memspace, H5S_SELECT_SET, offset, stride, count, blocks);
        if ( err < 0 ) {
            csc_error_message("Failed to select hyperslab.\n");
            ret = -1;
            goto end;
        }

        /* Do the hyperslab selection on the filespace   */
        offset[0] = 0;
        offset[1] = 0;
        stride[0] = 1;
        stride[1] = 1;
        count[0] = cols;
        if ( complex_as_compound ) {
            count[1] = rows;
        } else {
            count[1] = rows*cpx_stride;
        }
        blocks[0] = 1;
        blocks[1] = 1;
        err = H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, blocks);
        if ( err < 0 ) {
            csc_error_message("Failed to select hyperslab.\n");
            ret = -1;
            goto end;
        }

        if ( complex_as_compound) {
            size_t t = H5Tget_size(dtype);
            datatype_local = H5Tcreate(H5T_COMPOUND, 2*t);
            if ( datatype_local < 0 ) {
                csc_error_message("Failed to create complex type");
                ret = -1;
                goto end;
            }
            if (H5Tinsert(datatype_local, "real", 0, dtype) < 0) {
                ret = -1;
                goto end;
            }
            if (H5Tinsert(datatype_local, "imag", t, dtype) < 0) {
                ret = -1;
                goto end;
            }
            if (H5Tpack(datatype_local) < 0 ) {
                ret = -1;
                goto end;
            }
        } else {
            datatype_local = dtype;
        }

        err = H5Dread(dataset, datatype_local, memspace, filespace, H5P_DEFAULT, matrix );
        if ( err < 0 ) {
            csc_error_message("Read from dataset %s failed\n", dset_name);
            ret = -1;
            goto end;
        }
        ret = 0;

    } else if ( csc_hdf5_group_exist(root, dset_name)) {

        size_t tmpsize = strlen(dset_name) + 20;
        tmp = calloc(tmpsize, sizeof(char));
        if ( !tmp) {
            ret = -1;
            goto end;
        }

        snprintf(tmp, tmpsize, "%s/value", dset_name);
        if ( csc_hdf5_dataset_exist(root, tmp))  // Octave Matrix
        {
            ret = csc_hdf5_read_matrix_ex(root, tmp, dtype, rows, cols, matrix, ld, cpx_stride, opts);
            goto end;

        }

        snprintf(tmp, tmpsize, "%s/values", dset_name);
        if ( csc_hdf5_dataset_exist(root, tmp))  // Our old format
        {
            ret = csc_hdf5_read_matrix_ex(root, tmp, dtype, rows, cols, matrix, ld, cpx_stride, opts);
            goto end;

        }
    }


end:
    if ( memspace > 0) H5Sclose(memspace);
    if ( filespace > 0 ) H5Sclose(filespace);
    if ( datatype > 0 ) H5Tclose(datatype);
    if ( datatype_local != dtype ) H5Tclose(datatype_local);
    if ( dataset > 0 ) H5Dclose(dataset);
    if ( tmp )free( tmp );
    return ret;
}

int csc_hdf5_read_double_matrix( hid_t root, const char* dset_name, size_t rows, size_t cols, double * matrix, size_t lda , csc_hdf5_options_t * opts)
{
    CHECK_INPUT;
    return csc_hdf5_read_matrix_ex(root, dset_name, H5T_NATIVE_DOUBLE, rows, cols, (void *) matrix, lda, 1, opts);

}

int csc_hdf5_read_float_matrix( hid_t root, const char* dset_name, size_t rows, size_t cols, float * matrix, size_t lda , csc_hdf5_options_t * opts)
{
    CHECK_INPUT;
    return csc_hdf5_read_matrix_ex(root, dset_name, H5T_NATIVE_FLOAT, rows, cols, (void *) matrix, lda, 1, opts);
}

int csc_hdf5_read_double_complex_matrix( hid_t root, const char* dset_name, size_t rows, size_t cols, double complex * matrix, size_t lda , csc_hdf5_options_t * opts)
{
    CHECK_INPUT;
    return csc_hdf5_read_matrix_ex(root, dset_name, H5T_NATIVE_DOUBLE, rows, cols, (void *) matrix, lda, 2, opts);
}

int csc_hdf5_read_float_complex_matrix( hid_t root, const char* dset_name, size_t rows, size_t cols, float complex * matrix, size_t lda , csc_hdf5_options_t * opts)
{
    CHECK_INPUT;
    return csc_hdf5_read_matrix_ex(root, dset_name, H5T_NATIVE_FLOAT, rows, cols, (void *) matrix, lda, 2, opts);
}

int csc_hdf5_read_double_vector( hid_t root, const char* dset_name, size_t rows, double * matrix, csc_hdf5_options_t * opts)
{
    CHECK_INPUT_V;
    return csc_hdf5_read_matrix_ex(root, dset_name, H5T_NATIVE_DOUBLE, rows, 1, (void *) matrix, rows, 1, opts);

}

int csc_hdf5_read_float_vector( hid_t root, const char* dset_name, size_t rows, float * matrix, csc_hdf5_options_t * opts)
{
    CHECK_INPUT_V;
    return csc_hdf5_read_matrix_ex(root, dset_name, H5T_NATIVE_FLOAT, rows, 1, (void *) matrix, rows, 1, opts);
}

int csc_hdf5_read_double_complex_vector( hid_t root, const char* dset_name, size_t rows, double complex * matrix, csc_hdf5_options_t * opts)
{
    CHECK_INPUT_V;
    return csc_hdf5_read_matrix_ex(root, dset_name, H5T_NATIVE_DOUBLE, rows, 1, (void *) matrix, rows, 2, opts);
}

int csc_hdf5_read_float_complex_vector( hid_t root, const char* dset_name, size_t rows, float complex * matrix, csc_hdf5_options_t * opts)
{
    CHECK_INPUT_V;
    return csc_hdf5_read_matrix_ex(root, dset_name, H5T_NATIVE_FLOAT, rows, 1, (void *) matrix, rows, 2, opts);
}


/* Fortran Interface  */
int csc_hdf5_read_double_matrix_f( hid_t root, const char* dset_name, size_t rows, size_t cols, double * matrix, size_t lda)
{
    CHECK_INPUT;
    return csc_hdf5_read_matrix_ex(root, dset_name, H5T_NATIVE_DOUBLE, rows, cols, (void *) matrix, lda, 1, NULL);

}

int csc_hdf5_read_float_matrix_f( hid_t root, const char* dset_name, size_t rows, size_t cols, float * matrix, size_t lda)
{
    CHECK_INPUT;
    return csc_hdf5_read_matrix_ex(root, dset_name, H5T_NATIVE_FLOAT, rows, cols, (void *) matrix, lda, 1, NULL);
}

int csc_hdf5_read_double_complex_matrix_f( hid_t root, const char* dset_name, size_t rows, size_t cols, double complex * matrix, size_t lda )
{
    CHECK_INPUT;
    return csc_hdf5_read_matrix_ex(root, dset_name, H5T_NATIVE_DOUBLE, rows, cols, (void *) matrix, lda, 2, NULL);
}

int csc_hdf5_read_float_complex_matrix_f( hid_t root, const char* dset_name, size_t rows, size_t cols, float complex * matrix, size_t lda)
{
    CHECK_INPUT;
    return csc_hdf5_read_matrix_ex(root, dset_name, H5T_NATIVE_FLOAT, rows, cols, (void *) matrix, lda, 2, NULL);
}

int csc_hdf5_read_double_vector_f( hid_t root, const char* dset_name, size_t rows, double * matrix)
{
    CHECK_INPUT_V;
    return csc_hdf5_read_matrix_ex(root, dset_name, H5T_NATIVE_DOUBLE, rows, 1, (void *) matrix, rows, 1, NULL);

}

int csc_hdf5_read_float_vector_f( hid_t root, const char* dset_name, size_t rows, float * matrix)
{
    CHECK_INPUT_V;
    return csc_hdf5_read_matrix_ex(root, dset_name, H5T_NATIVE_FLOAT, rows, 1,(void *) matrix, rows, 1, NULL);
}

int csc_hdf5_read_double_complex_vector_f( hid_t root, const char* dset_name, size_t rows, double complex * matrix)
{
    CHECK_INPUT_V;
    return csc_hdf5_read_matrix_ex(root, dset_name, H5T_NATIVE_DOUBLE, rows, 1,(void *) matrix, rows, 2, NULL);
}

int csc_hdf5_read_float_complex_vector_f( hid_t root, const char* dset_name, size_t rows, float complex * matrix)
{
    CHECK_INPUT_V;
    return csc_hdf5_read_matrix_ex(root, dset_name, H5T_NATIVE_FLOAT, rows, 1, (void *) matrix, rows, 2, NULL);
}

#undef FORTRAN_COMPRESS
#undef CHECK_INPUT
#undef CHECK_INPUT_V
/* Check if the given dataset is a dense matrix.  */
int csc_hdf5_is_matrix(hid_t root, const char *dset_name)
{
    char *tmp = NULL;
    hid_t dataset_id = 0;
    hid_t dataspace_id = 0;
    int ret = 0;
    int rank = 0;

    if ( !dset_name) return 0;

    if ( csc_hdf5_dataset_exist(root,dset_name)) {
        /* Matrix is a dataset -> New format  */
        /* Everything which is one or two dimensional can be interpreted as a matrix.  */
        /* It works also for MATLAB v7.3 files.  */
        dataset_id = H5Dopen(root, dset_name, H5P_DEFAULT);
        if ( dataset_id < 0 ) {
            ret = 0;
            goto end;
        }

        dataspace_id = H5Dget_space(dataset_id);
        if ( dataset_id < 0 ) {
            ret = 0;
            goto end;
        }

        rank = H5Sget_simple_extent_ndims(dataspace_id);
        if ( rank < 0 ) {
            ret = 0;
            goto end;
        }
        if ( rank == 2 ) {
            ret = 1;
        }

    } else if ( csc_hdf5_group_exist(root,dset_name)) {
        size_t tmpsize = strlen(dset_name) + 20;
        tmp = calloc(tmpsize, sizeof(char));
        if ( !tmp) {
            ret = 0;
            goto end;
        }

        snprintf(tmp, tmpsize, "%s/type", dset_name);
        if ( csc_hdf5_dataset_exist(root, tmp)) {
            /* It is an OCTAVE HDF5 file */
            snprintf(tmp, tmpsize, "%s/value", dset_name);

            dataset_id = H5Dopen(root, tmp, H5P_DEFAULT);
            if ( dataset_id < 0 ) {
                ret = 0;
                goto end;
            }
            dataspace_id = H5Dget_space(dataset_id);
            if ( dataset_id < 0 ) {
                ret = 0;
                goto end;
            }

            rank = H5Sget_simple_extent_ndims(dataspace_id);
            if ( rank < 0 ) {
                ret = 0;
                goto end;
            }
            if ( rank == 2 ) {
                ret = 1;
            }
            goto end;
        }

        csc_hdf5_content_t content;
        if ( csc_hdf5_get_content(root, dset_name, &content) == 0 ) {
            if (content == CSC_HDF5_MATRIX )
                ret = 1;
            else
                ret = 0;
        } else {
            ret = 0;
        }
    }

end:
    if ( dataspace_id > 0 ) H5Dclose(dataspace_id);
    if ( dataset_id > 0 )  H5Sclose(dataset_id);
    if ( tmp ) free(tmp);
    return ret;

}


/* Read the size of a matrix from  a data set   */
int csc_hdf5_matrix_size(hid_t root, const char *dset_name, size_t *rows, size_t *cols)
{
    int ret = 0;
    hid_t dataset_id = 0;
    hid_t dataspace_id = 0;
    int rank = -1;
    int cpx = 0 ;
    hsize_t dims[2];
    char * tmp = NULL;

    /* Check input  */
    if (! dset_name) {
        ret = -1;
        goto end;
    }
    if ( ! rows ) {
        ret = -1;
        goto end;
    }
    if ( ! cols ) {
        ret = -1;
        goto end;
    }


    if ( csc_hdf5_dataset_exist(root,dset_name)) {
        /* Matrix is a dataset -> New format  */
        /* Everything which is one or two dimensional can be interpreted as a matrix.  */
        /* It works also for MATLAB v7.3 files.  */

        dataset_id = H5Dopen(root, dset_name, H5P_DEFAULT);
        if ( dataset_id < 0 ) {
            ret = -1;
            goto end;
        }

        dataspace_id = H5Dget_space(dataset_id);
        if ( dataset_id < 0 ) {
            ret = -1;
            goto end;
        }

        rank = H5Sget_simple_extent_ndims(dataspace_id);
        if ( rank < 0 ) {
            ret = -1;
            goto end;
        }
        if ( rank > 2 || rank <= 0 ) {
            ret = -1;
            goto end;
        }
        ret = H5Sget_simple_extent_dims(dataspace_id, dims, NULL);
        if ( ret < 0 ) {
            ret = -1;
            goto end;
        }
        if ( rank == 1 ) {
             *rows = dims[0];
             *cols = 1;
        } else {
            *rows = dims[1];
            *cols = dims[0];
        }

        if ( csc_hdf5_attribute_exist(root, dset_name, "COMPLEX") &&
             csc_hdf5_attribute_read_int(root, dset_name, "COMPLEX", &cpx) == 0 ) {
            if ( cpx ) *rows /= 2;
        }
        ret = 0;
        goto end;

    } else if ( csc_hdf5_group_exist(root,dset_name)) {
        /* Matrix is a group -> old format   */
        /* Also works with Octave  */
        size_t tmpsize = strlen(dset_name) + 20;
        tmp = calloc(tmpsize, sizeof(char));
        if ( !tmp) {
            ret = -1;
            goto end;
        }

        snprintf(tmp, tmpsize, "%s/type", dset_name);
        if ( csc_hdf5_dataset_exist(root, tmp)) {
            /* It is an OCTAVE HDF5 file */
            snprintf(tmp, tmpsize, "%s/value", dset_name);

            dataset_id = H5Dopen(root, tmp, H5P_DEFAULT);
            if ( dataset_id < 0 ) {
                ret = -1;
                goto end;
            }
            dataspace_id = H5Dget_space(dataset_id);
            if ( dataset_id < 0 ) {
                ret = -1;
                goto end;
            }

            rank = H5Sget_simple_extent_ndims(dataspace_id);
            if ( rank < 0 ) {
                ret = -1;
                goto end;
            }
            if ( rank > 2 || rank <= 0 ) {
                ret = -1;
                goto end;
            }
            ret = H5Sget_simple_extent_dims(dataspace_id, dims, NULL);
            if ( ret < 0 ) {
                ret = -1;
                goto end;
            }
            if ( rank == 1 ) {
                *rows = dims[0];
                *cols = 1;
            } else {
                *rows = dims[1];
                *cols = dims[0];
            }
            ret = 0;
            goto end;
        }

        /* Our old format  */
        if (! csc_hdf5_is_matrix(root, dset_name)){
            csc_error_message("Dataset %s is not a matrix.\n", dset_name);
            ret = -1;
            goto end;
        }
        unsigned long r, c;

        if ( csc_hdf5_attribute_read_ulong(root, dset_name, "rows", &r) < 0 ) {
            ret = -1;
            goto end;
        }
        if ( csc_hdf5_attribute_read_ulong(root, dset_name, "cols", &c) < 0 ) {
            ret = -1;
            goto end;
        }
        *rows = r;
        *cols = c;
        ret = 0;
    } else {
        ret = -1;
    }
end:
    if (dataspace_id > 0 ) H5Sclose(dataspace_id);
    if (dataset_id > 0 )  H5Dclose(dataset_id);
    if (tmp) free(tmp);

    return ret;
}

int csc_hdf5_matrix_get_datatype(hid_t root, const char *dset_name, csc_hdf5_field_t *field)
{
    hid_t dataset_id = 0;
    hid_t datatype_id = 0;
    hid_t native_id = 0;
    hid_t rcomplex_type = 0;
    hid_t icomplex_type = 0;
    hid_t native_rcomplex_type = 0;
    hid_t native_icomplex_type = 0;

    H5T_class_t typeclass = H5T_NO_CLASS;
    char * tmp = NULL;
    int ret = -1;

    *field = CSC_HDF5_UNKOWN_FIELD;

    if ( csc_hdf5_dataset_exist(root,dset_name)) {
        /* New and MATLAB */
        dataset_id = H5Dopen(root, dset_name, H5P_DEFAULT);
        if ( dataset_id < 0 ) {
            ret = -1;
            goto end;
        }

        datatype_id = H5Dget_type(dataset_id);
        if ( datatype_id < 0 ) {
            ret = -1;
            goto end;
        }

        typeclass = H5Tget_class(datatype_id);
        if ( typeclass == H5T_NO_CLASS) {
            ret = -1;
            goto end;
        }

        if ( typeclass == H5T_INTEGER ) {
            native_id = H5Tget_native_type(datatype_id, H5T_DIR_ASCEND);
            if ( H5Tequal(native_id, H5T_NATIVE_CHAR) > 0 && sizeof(char) == 1 ) {
                *field = CSC_HDF5_INTEGER8;
            } else if ( H5Tequal(native_id, H5T_NATIVE_SHORT) > 0 && sizeof(short) == 2) {
                *field = CSC_HDF5_INTEGER16;
            } else if ( H5Tequal(native_id, H5T_NATIVE_INT) > 0 && sizeof(int) == 4 ) {
                 *field = CSC_HDF5_INTEGER32;
            } else if ( H5Tequal(native_id, H5T_NATIVE_LONG) > 0 && sizeof(long) == 8) {
                *field = CSC_HDF5_INTEGER64;
            } else {
                *field = CSC_HDF5_INTEGER32;
            }
            ret = 0;
            goto end;
        } else if( typeclass == H5T_FLOAT) {
            native_id = H5Tget_native_type(datatype_id, H5T_DIR_ASCEND);
            if ( native_id < 0 ) {
                ret = -1;
                goto end;
            }
            if (H5Tequal(native_id, H5T_NATIVE_FLOAT) > 0 ) {
                int cpx = 0;
                *field = CSC_HDF5_FLOAT;
                if ( csc_hdf5_attribute_exist(root, dset_name, "COMPLEX") &&
                     csc_hdf5_attribute_read_int(root , dset_name, "COMPLEX", &cpx) == 0 ) {
                    if (cpx == 1) {
                        *field = CSC_HDF5_FLOAT_COMPLEX;
                    }
                }
                ret = 0;
            } else if ( H5Tequal(native_id, H5T_NATIVE_DOUBLE) > 0 ) {
                int cpx = 0;
                *field = CSC_HDF5_DOUBLE;
                if ( csc_hdf5_attribute_exist(root, dset_name, "COMPLEX") &&
                     csc_hdf5_attribute_read_int(root, dset_name, "COMPLEX", &cpx) == 0 ) {
                    if (cpx == 1) {
                        *field = CSC_HDF5_DOUBLE_COMPLEX;
                    }
                }
                ret = 0;
            } else {
                ret = -1;
            }
            goto end;
        } else if ( typeclass == H5T_COMPOUND ) {
            /* Complex compound type  */
            if ( H5Tget_nmembers(datatype_id) != 2 ) {
                ret = -1;
                goto end;
            }
            if (H5Tget_member_class(datatype_id, 0) != H5T_FLOAT ||
                H5Tget_member_class(datatype_id, 1) != H5T_FLOAT ) {
                ret = -1;
                goto end;
            }

            rcomplex_type = H5Tget_member_type(datatype_id, 0);
            icomplex_type = H5Tget_member_type(datatype_id, 1);
            if ( rcomplex_type < 0 || icomplex_type < 0 ) {
                ret = -1;
                goto end;
            }
            native_rcomplex_type = H5Tget_native_type(rcomplex_type, H5T_DIR_ASCEND);
            native_icomplex_type = H5Tget_native_type(icomplex_type, H5T_DIR_ASCEND);
            if ( native_icomplex_type < 0 || native_rcomplex_type < 0 ) {
                ret = -1;
                goto end;
            }

            if ( H5Tequal(native_rcomplex_type, H5T_NATIVE_DOUBLE) > 0 && H5Tequal(native_icomplex_type, H5T_NATIVE_DOUBLE) > 0 ) {
                *field = CSC_HDF5_DOUBLE_COMPLEX;
                ret = 0;
            } else if (H5Tequal(native_rcomplex_type, H5T_NATIVE_FLOAT) > 0 && H5Tequal(native_icomplex_type, H5T_NATIVE_FLOAT) > 0) {
                *field = CSC_HDF5_FLOAT_COMPLEX;
                ret = 0;
            } else {
                ret = -1;
            }

        } else {
            ret = -1;
            goto end;
        }
    } else {
        /* Old and Octave */
        int err = 0;
        size_t tmpsize = strlen(dset_name) + 20;
        tmp = calloc(tmpsize, sizeof(char));
        if ( !tmp) {
            ret = -1;
            goto end;
        }

        snprintf(tmp, tmpsize, "%s/value", dset_name);    // Name of the Octave set.
        if ( csc_hdf5_dataset_exist(root, tmp)) {
            err = csc_hdf5_matrix_get_datatype(root, tmp, field);
            ret = 0;
            if ( err ) ret = -1;
            goto end;
        }

        if (csc_hdf5_attribute_exist(root, dset_name, "csc_hdf5_field")) {
            err = csc_hdf5_get_field(root, dset_name, field);
            if ( err ) {
                ret = -1;
            } else {
                ret = 0;
            }
            goto end;
        }
        ret = -1;
    }
end:
    if (dataset_id > 0)   H5Dclose(dataset_id);
    if (datatype_id > 0 ) H5Tclose(datatype_id);
    if (native_id > 0)    H5Tclose(native_id);
    if (rcomplex_type > 0 ) H5Tclose(rcomplex_type);
    if (icomplex_type > 0 ) H5Tclose(icomplex_type);
    if (native_rcomplex_type > 0 ) H5Tclose(native_rcomplex_type);
    if (native_icomplex_type > 0 ) H5Tclose(native_icomplex_type);

    if (tmp) free(tmp);
    return ret;
}

/*
 *
 * Old Style Routines
 *
 * */

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
        hid_t       memspace_id;
        hid_t       dataspace_id;
        hid_t       dataset_id;
        /* printf("hier\n"); */
        err = csc_hdf5_create_dataset(vg, "values", 2, dims, H5T_NATIVE_DOUBLE, NULL);
        if ( err < 0 ) {
            csc_error_message("Failed to write a real matrix of size (%lu, %lu) to %s\n", (unsigned long) rows, (unsigned long) cols, dset_name);
            H5Gclose(vg);
            return -1;
        }
        dataset_id = H5Dopen2 (vg, "values", H5P_DEFAULT);
        if ( dataset_id < 0 ) {
            csc_error_message("Failed to open the data space from %s.\n", dset_name);
            H5Gclose(vg);
            return -1;
        }

        dataspace_id = H5Dget_space(dataset_id);
        if ( dataspace_id < 0 ) {
            csc_error_message("Failed to get the data space from %s\n", dset_name);
            H5Dclose(dataset_id);
            H5Gclose(vg);
            return -1;
        }

        offset[0] = 0;
        offset[1] = 0;
        stride[0] = 1;
        stride[1] = 1;
        count[0] = cols;
        count[1] = rows;
        blocks[0] = 1;
        blocks[1] = 1;


        dim_block[0] = cols;
        dim_block[1] = ld;
        memspace_id = H5Screate_simple (2, dim_block, NULL);
        if ( memspace_id < 0 ) {
            csc_error_message("Failed to create memory data space.\n");
            H5Sclose(dataspace_id);
            H5Dclose(dataset_id);
            H5Gclose(vg);
            return -1;
        }

        err = H5Sselect_hyperslab(memspace_id, H5S_SELECT_SET, offset, stride, count, blocks);
        if ( err < 0 ) {
            csc_error_message("Failed to select (rows x cols) block from ( ld x cols) spaces.\n");
            H5Sclose(memspace_id);
            H5Sclose(dataspace_id);
            H5Dclose(dataset_id);
            H5Gclose(vg);
            return -1;
        }


        err  = H5Dwrite (dataset_id, H5T_NATIVE_DOUBLE, memspace_id, dataspace_id, H5P_DEFAULT, matrix);
        if ( err < 0 )     {
            csc_error_message("Failed to write to %s\n", dset_name);
            H5Dclose(dataset_id);
            H5Sclose(memspace_id);
            H5Sclose(dataspace_id);
            H5Gclose(vg);
            return -1;
        }

        H5Sclose(memspace_id);
        H5Sclose(dataspace_id);
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
        hid_t       memspace_id;
        hid_t       dataspace_id;
        hid_t       dataset_id;
        /* printf("hier\n"); */
        err = csc_hdf5_create_dataset(vg, "values", 2, dims, H5T_NATIVE_FLOAT, NULL);
        if ( err < 0 ) {
            csc_error_message("Failed to write a real matrix of size (%lu, %lu) to %s\n", (unsigned long) rows, (unsigned long) cols, dset_name);
            H5Gclose(vg);
            return -1;
        }
        dataset_id = H5Dopen2 (vg, "values", H5P_DEFAULT);
        if ( dataset_id < 0 ) {
            csc_error_message("Failed to open the data space from %s.\n", dset_name);
            H5Gclose(vg);
            return -1;
        }

        dataspace_id = H5Dget_space(dataset_id);
        if ( dataspace_id < 0 ) {
            csc_error_message("Failed to get the data space from %s\n", dset_name);
            H5Dclose(dataset_id);
            H5Gclose(vg);
            return -1;
        }

        offset[0] = 0;
        offset[1] = 0;
        stride[0] = 1;
        stride[1] = 1;
        count[0] = cols;
        count[1] = rows;
        blocks[0] = 1;
        blocks[1] = 1;


        dim_block[0] = cols;
        dim_block[1] = ld;
        memspace_id = H5Screate_simple (2, dim_block, NULL);
        if ( memspace_id < 0 ) {
            csc_error_message("Failed to create memory data space.\n");
            H5Sclose(dataspace_id);
            H5Dclose(dataset_id);
            H5Gclose(vg);
            return -1;
        }

        err = H5Sselect_hyperslab(memspace_id, H5S_SELECT_SET, offset, stride, count, blocks);
        if ( err < 0 ) {
            csc_error_message("Failed to select (rows x cols) block from ( ld x cols) spaces.\n");
            H5Sclose(memspace_id);
            H5Sclose(dataspace_id);
            H5Dclose(dataset_id);
            H5Gclose(vg);
            return -1;
        }


        err  = H5Dwrite (dataset_id, H5T_NATIVE_FLOAT, memspace_id, dataspace_id, H5P_DEFAULT, matrix);
        if ( err < 0 )     {
            csc_error_message("Failed to write to %s\n", dset_name);
            H5Dclose(dataset_id);
            H5Sclose(memspace_id);
            H5Sclose(dataspace_id);
            H5Gclose(vg);
            return -1;
        }

        H5Sclose(memspace_id);
        H5Sclose(dataspace_id);
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
        hid_t       memspace_id;
        hid_t       dataspace_id;
        hid_t       dataset_id;
        /* printf("hier\n"); */
        err = csc_hdf5_create_dataset(vg, "values", 2, dims, complex_type, NULL);
        if ( err < 0 ) {
            csc_error_message("Failed to write a real matrix of size (%lu, %lu) to %s\n", (unsigned long) rows, (unsigned long) cols, dset_name);
            H5Gclose(vg);
            return -1;
        }
        dataset_id = H5Dopen2 (vg, "values", H5P_DEFAULT);
        if ( dataset_id < 0 ) {
            csc_error_message("Failed to open the data space from %s.\n", dset_name);
            H5Gclose(vg);
            return -1;
        }

        dataspace_id = H5Dget_space(dataset_id);
        if ( dataspace_id < 0 ) {
            csc_error_message("Failed to get the data space from %s\n", dset_name);
            H5Dclose(dataset_id);
            H5Gclose(vg);
            return -1;
        }

        offset[0] = 0;
        offset[1] = 0;
        stride[0] = 1;
        stride[1] = 1;
        count[0] = cols;
        count[1] = rows;
        blocks[0] = 1;
        blocks[1] = 1;


        dim_block[0] = cols;
        dim_block[1] = ld;
        memspace_id = H5Screate_simple (2, dim_block, NULL);
        if ( memspace_id < 0 ) {
            csc_error_message("Failed to create memory data space.\n");
            H5Sclose(dataspace_id);
            H5Dclose(dataset_id);
            H5Gclose(vg);
            return -1;
        }

        err = H5Sselect_hyperslab(memspace_id, H5S_SELECT_SET, offset, stride, count, blocks);
        if ( err < 0 ) {
            csc_error_message("Failed to select (rows x cols) block from ( ld x cols) spaces.\n");
            H5Sclose(memspace_id);
            H5Sclose(dataspace_id);
            H5Dclose(dataset_id);
            H5Gclose(vg);
            return -1;
        }


        err  = H5Dwrite (dataset_id, complex_type, memspace_id, dataspace_id, H5P_DEFAULT, matrix);
        if ( err < 0 )     {
            csc_error_message("Failed to write to %s\n", dset_name);
            H5Dclose(dataset_id);
            H5Sclose(memspace_id);
            H5Sclose(dataspace_id);
            H5Gclose(vg);
            return -1;
        }

        H5Sclose(memspace_id);
        H5Sclose(dataspace_id);
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
    int tmp;


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

    tmp = H5Sget_simple_extent_ndims(dataspace_id);
    if( tmp < 0 ) {
        csc_error_message("Failed to get the rank of the data set %s.\n", dset_name);
        H5Dclose(dataspace_id);
        H5Sclose(dataset_id);
        H5Gclose(vg);

        return -1;
    } else {
        rank = tmp;
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
        hid_t       memspace_id;
        hid_t       dataspace_loc_id;

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
    int tmp;

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

    tmp = H5Sget_simple_extent_ndims(dataspace_id);
    if( tmp < 0 ) {
        csc_error_message("Failed to get the rank of the data set %s.\n", dset_name);
        H5Dclose(dataspace_id);
        H5Sclose(dataset_id);
        H5Gclose(vg);

        return -1;
    } else {
        rank = tmp;
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
        hid_t       memspace_id;
        hid_t       dataspace_loc_id;

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
    int tmp;

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

    tmp = H5Sget_simple_extent_ndims(dataspace_id);

    if( tmp  < 0) {
        csc_error_message("Failed to get the rank of the data set %s.\n", dset_name);
        H5Dclose(dataspace_id);
        H5Sclose(dataset_id);
        H5Tclose(complex_type);

        return -1;
    } else {
        rank = tmp;
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
        hid_t       memspace_id;
        hid_t       dataspace_loc_id;

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



