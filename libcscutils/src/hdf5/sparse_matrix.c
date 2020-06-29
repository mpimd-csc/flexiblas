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
    int ret = -1;
    char * tmp = NULL;
    hid_t dataset_id = 0;
    hid_t space_id = 0;
    hid_t group_id = 0;
    hid_t val_group_id = 0;

    if (    csc_hdf5_is_spcsr(root,dset_name)
            ||  csc_hdf5_is_spcsc(root,dset_name)
            ||  csc_hdf5_is_spcoo(root,dset_name) )
    {
        HDF5_GET_ATTR_ULONG(root,dset_name,"rows",*rows);
        HDF5_GET_ATTR_ULONG(root,dset_name,"cols",*cols);
        HDF5_GET_ATTR_ULONG(root,dset_name,"nnz",*nnz);

        ret = 0;
        goto end;
    }  else if ( csc_hdf5_is_sparse(root, dset_name) && csc_hdf5_is_octave(root, dset_name)) {

        unsigned long nr, nz, nc;
        // Octave
        group_id = H5Gopen2(root, dset_name, H5P_DEFAULT);
        if ( group_id < 0 ) {
            csc_error_message("Failed to open %s\n", dset_name);
            goto end;
        }
        val_group_id = H5Gopen2(group_id, "value", H5P_DEFAULT);
        if ( val_group_id < 0 ) {
            csc_error_message("Failed to open %s/value\n", dset_name);
            goto end;
        }

        if (csc_hdf5_read_ulong(val_group_id, "nr", &nr)) {
            csc_error_message("Failed to read the number of rows from %s\n", dset_name);
            goto end;
        }
        *rows = (size_t) nr;

        if (csc_hdf5_read_ulong(val_group_id, "nc", &nc)) {
            csc_error_message("Failed to read the number of columns from %s\n", dset_name);
            goto end;
        }
        *cols = (size_t) nc;

        if (csc_hdf5_read_ulong(val_group_id, "nz", &nz)) {
            csc_error_message("Failed to read the number of non-zeros from %s\n", dset_name);
            goto end;
        }
        *nnz = (size_t) nz;

        H5Gclose(val_group_id); val_group_id = 0;
        H5Gclose(group_id); group_id = 0;
        ret = 0;
    } else if (csc_hdf5_is_sparse(root, dset_name) ) {
        // MATLAB
        size_t maxlen = strlen(dset_name) + 19;
        int rank;
        int err;
        hsize_t dims[2];

        tmp = (char *) malloc(sizeof(char) * (strlen(dset_name)+20));
        if ( !tmp ) { ret = -1; goto end; }

        // The number of rows is encoded in MATLAB_sparse
        if ( csc_hdf5_attribute_exist(root, dset_name, "MATLAB_sparse")) {
            unsigned long nr;
            if ( csc_hdf5_attribute_read_ulong(root, dset_name, "MATLAB_sparse", &nr)) {
                csc_error_message("MATLAB_sparse could not be read.");
                ret = -1;
                goto end;
            }
            *rows = (size_t) nr;
        } else {
            unsigned long nr;
            if (!csc_hdf5_attribute_exist(root, dset_name, "ROWS")) {
                csc_error_message("ROWS attribute not found in dataset %s.", dset_name);
                ret = -1;
                goto end;
            }
            if (csc_hdf5_attribute_read_ulong(root, dset_name, "ROWS", &nr)) {
                csc_error_message("ROWS attribute could not be read.");
                ret = -1;
                goto end;
            }
            *rows = (size_t) nr;
        }

        // The number of columns is len(jc)-1
        snprintf(tmp, maxlen, "%s/jc", dset_name);
        dataset_id = H5Dopen2(root, tmp, H5P_DEFAULT);
        if ( dataset_id < 0 ) goto end;

        space_id = H5Dget_space(dataset_id);
        if ( space_id < 0 ) goto end;

        rank = H5Sget_simple_extent_ndims(space_id);
        if ( rank < 0 || rank > 1) goto end;
        err = H5Sget_simple_extent_dims(space_id, dims, NULL);
        if ( err < 0 ) goto end;
        *cols = dims[0]-1;


        H5Sclose(space_id);
        space_id = 0;
        H5Dclose(dataset_id);
        dataset_id = 0;

        // The number of nnz is len(ir)
        snprintf(tmp, maxlen, "%s/ir", dset_name);
        dataset_id = H5Dopen2(root, tmp, H5P_DEFAULT);
        if ( dataset_id < 0 ) goto end;

        space_id = H5Dget_space(dataset_id);
        if ( space_id < 0 ) goto end;

        rank = H5Sget_simple_extent_ndims(space_id);
        if ( rank < 0 || rank > 1) goto end;
        err = H5Sget_simple_extent_dims(space_id, dims, NULL);
        if ( err < 0 ) goto end;
        *nnz = dims[0];


        H5Sclose(space_id);
        space_id = 0;
        H5Dclose(dataset_id);
        dataset_id = 0;


        ret = 0;
    } else {
        csc_error_message("Matrix not sparse");
        ret = -1;
    }
end:
    if ( tmp ) free(tmp);
    if ( space_id > 0 ) H5Sclose(space_id);
    if ( dataset_id > 0 ) H5Dclose(dataset_id);
    if ( val_group_id > 0 ) H5Gclose(val_group_id);
    if ( group_id > 0 ) H5Gclose(group_id);
    return ret;

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

int csc_hdf5_is_sparse(hid_t root, const char *dset_name)
{
  hid_t group_id = 0;
        char *type_str = NULL;
    size_t len = 0;

    if ( ! csc_hdf5_group_exist(root, dset_name)) {
        return 0;
    }
    if ( csc_hdf5_attribute_exist(root, dset_name, "MATLAB_sparse")) {
        return 1;
    }
    if ( csc_hdf5_is_octave(root,dset_name)) {

        group_id = H5Gopen2(root, dset_name, H5P_DEFAULT);
        if (group_id < 0) goto end;
        if ( csc_hdf5_read_string(group_id, "type", &type_str, &len)) {

            goto end;
        }
        if (strcmp(type_str, "sparse complex matrix") == 0
            || strcmp(type_str, "sparse matrix") == 0){
            free(type_str);
            H5Gclose(group_id);
            return 1;
        }
    }

    group_id = H5Gopen2(root, dset_name, H5P_DEFAULT);
    if ( group_id < 0 ) goto end;
    int issparse = 0;
    if (  csc_hdf5_dataset_exist(group_id, "ir")
         && csc_hdf5_dataset_exist(group_id, "jc")
         && csc_hdf5_dataset_exist(group_id, "data")) {
        issparse = 1;
    }
    H5Gclose(group_id);
    return issparse;


end:
    if ( group_id > 0 ) H5Gclose(group_id);
    if ( type_str ) free(type_str);
    return 0;
}


int csc_hdf5_sparse_get_datatype(hid_t root, const char *dset_name, csc_hdf5_field_t *field)
{
    int ret = -1;
    hid_t dataset_id = 0;
    hid_t datatype_id = 0;
    hid_t native_id = 0;
    hid_t rcomplex_type = 0;
    hid_t icomplex_type = 0;
    hid_t native_rcomplex_type = 0;
    hid_t native_icomplex_type = 0;
    hid_t group_id = 0;

    H5T_class_t typeclass = H5T_NO_CLASS;
    char * tmp = NULL;

    *field = CSC_HDF5_UNKOWN_FIELD;


    /*  Handle old types */
    if (    csc_hdf5_is_spcsr(root,dset_name)
            ||  csc_hdf5_is_spcsc(root,dset_name)
            ||  csc_hdf5_is_spcoo(root,dset_name) )
    {
        return csc_hdf5_get_field(root, dset_name, field);

    }else if ( csc_hdf5_is_sparse(root, dset_name) && csc_hdf5_is_octave(root, dset_name)) {
        size_t maxlen = 0;

        group_id = H5Gopen2(root, dset_name, H5P_DEFAULT);
        if ( group_id < 0 ) {
            ret = -1;
            goto end;
        }

        if ( csc_hdf5_read_string(group_id, "type", &tmp, &maxlen)) {
            ret = -1;
            goto end;
        }

        if ( strcmp(tmp, "sparse matrix") == 0 ){
            *field = CSC_HDF5_DOUBLE;
        } else if ( strcmp(tmp, "sparse complex matrix") == 0) {
            *field = CSC_HDF5_DOUBLE_COMPLEX;
        } else {
            ret = -1;
            goto end;
        }
        ret = 0;
    }   else if (csc_hdf5_is_sparse(root, dset_name) ) {
        // MATLAB
        size_t maxlen = strlen(dset_name) + 20;
        tmp = (char *) malloc(sizeof(char) * (maxlen));

        snprintf(tmp, maxlen, "%s/data", dset_name);

        dataset_id = H5Dopen(root, tmp, H5P_DEFAULT);
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
                *field = CSC_HDF5_FLOAT;

                // If we come up with our own format
                int cpx = 0;
                if ( csc_hdf5_attribute_exist(root, tmp, "COMPLEX") &&
                     csc_hdf5_attribute_read_int(root , tmp, "COMPLEX", &cpx) == 0 ) {
                    if (cpx == 1) {
                        *field = CSC_HDF5_FLOAT_COMPLEX;
                    }
                }
                ret = 0;
            } else if ( H5Tequal(native_id, H5T_NATIVE_DOUBLE) > 0 ) {
                *field = CSC_HDF5_DOUBLE;

                /* If we come up with our own format */
                int cpx = 0;
                if ( csc_hdf5_attribute_exist(root, tmp, "COMPLEX") &&
                     csc_hdf5_attribute_read_int(root, tmp, "COMPLEX", &cpx) == 0 ) {
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
    if (group_id > 0 ) H5Gclose(group_id);
    if (tmp) free(tmp);
    return ret;
}


static int csc_hdf5_write_sparse_ex(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz,
        hid_t dtype_rows, void *rowptr,
        hid_t dtype_cols, void *colptr,
        hid_t dtype_data, void *data,
        int cpx_stride, csc_hdf5_options_t *opts)
{

    hid_t group_id = 0;
    hid_t val_group_id = 0;
    hid_t memspace_id = 0;
    hid_t dataset_id = 0;
    hid_t dtype_local = 0;
    herr_t err = 0;
    int ret = -1;
    csc_hdf5_options_t default_opts = CSC_HDF5_OPTIONS_INIT;
    hsize_t dims [2];

    if ( opts == NULL) {
        opts = &default_opts;
    }
    if ( opts->compress != CSC_HDF5_COMPRESS_NONE  && opts->chunksize == 0 ) {
        csc_error_message("enabled compression needs a chunksize > 0 \n");
        return -1;
    }

    if ( csc_hdf5_group_path_create(root, dset_name, 1)) {
        csc_error_message("Failed to create path -- %s.\n", dset_name);
        ret = -1;
        goto end;
    }

    if ( opts -> octave_style ) {
        // Octave compatible
        unsigned long nr, nc, nz;

        if ( csc_hdf5_attribute_write_int(root, dset_name, "OCTAVE_NEW_FORMAT", 1)) goto end;

        group_id = H5Gopen2(root, dset_name, H5P_DEFAULT);
        if ( group_id < 0 ) goto end;


        if ( cpx_stride != 1) {
            if (csc_hdf5_write_string(group_id, "type", "sparse complex matrix")) goto end;
        } else {
            if (csc_hdf5_write_string(group_id, "type", "sparse matrix")) goto end;
        }

        if ( csc_hdf5_group_exist(group_id, "value")) {
            if ( H5Ldelete(group_id, "value", H5P_DEFAULT) < 0 ) {
                csc_error_message("Failed to remove previous entry %s/value\n", dset_name);
                ret = -1;
                goto end;
            }
        }

        val_group_id =  H5Gcreate2(group_id, "value", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        if ( val_group_id < 0 ) goto end;

        nr = rows;
        nc = cols;
        nz = nnz;
        if ( csc_hdf5_write_ulong(val_group_id, "nr", nr)) goto end;
        if ( csc_hdf5_write_ulong(val_group_id, "nc", nc)) goto end;
        if ( csc_hdf5_write_ulong(val_group_id, "nz", nz)) goto end;

        // CIDX Vector length cols+1
        if ( H5Lexists(val_group_id, "cidx", H5P_DEFAULT) > 0 ) {
            if ( H5Ldelete(val_group_id, "cidx", H5P_DEFAULT) < 0 ) {
                csc_error_message("Failed to remove previous entry %s/value/cidx\n", dset_name);
                ret = -1;
                goto end;
            }
        }

        dims[0] = cols + 1;
        dims[1] = 1;
        memspace_id = H5Screate_simple(2, dims, NULL);
        if ( memspace_id < 0 ) {
            csc_error_message("Failed to create memspace.\n");
            ret = -1;
            goto end;
        }

        dataset_id = H5Dcreate2(val_group_id, "cidx", dtype_cols, memspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        if ( dataset_id < 0 ) {
            csc_error_message("Failed to create dataset %s/value/cidx\n", dset_name);
            ret = -1;
            goto end;
        }
        err = H5Dwrite(dataset_id, dtype_cols, memspace_id, memspace_id, H5P_DEFAULT, colptr);
        if ( err < 0 ) {
            csc_error_message("Failed to write to %s/value/cidx\n", dset_name);
            ret = -1;
            goto end;
        }
        H5Dclose(dataset_id);
        H5Sclose(memspace_id);

        // RIDX Vector length nnz
        if ( H5Lexists(val_group_id, "ridx", H5P_DEFAULT) > 0 ) {
            if ( H5Ldelete(val_group_id, "ridx", H5P_DEFAULT) < 0 ) {
                csc_error_message("Failed to remove previous entry %s/value/ridx\n", dset_name);
                ret = -1;
                goto end;
            }
        }

        dims[0] = nnz;
        dims[1] = 1;
        memspace_id = H5Screate_simple(2, dims, NULL);
        if ( memspace_id < 0 ) {
            csc_error_message("Failed to create memspace.\n");
            ret = -1;
            goto end;
        }

        dataset_id = H5Dcreate2(val_group_id, "ridx", dtype_rows, memspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        if ( dataset_id < 0 ) {
            csc_error_message("Failed to create dataset %s/value/ridx\n", dset_name);
            ret = -1;
            goto end;
        }
        err = H5Dwrite(dataset_id, dtype_rows, memspace_id, memspace_id, H5P_DEFAULT, rowptr);
        if ( err < 0 ) {
            csc_error_message("Failed to write to %s/value/ridx\n", dset_name);
            ret = -1;
            goto end;
        }
        H5Dclose(dataset_id);
        H5Sclose(memspace_id);

        // Data
        if ( cpx_stride > 1 ) {
            size_t t = H5Tget_size(dtype_data);
            dtype_local = H5Tcreate(H5T_COMPOUND, 2*t);
            if ( dtype_local < 0 ) {
                csc_error_message("Failed to create complex type");
                ret = -1;
                goto end;
            }
            if (H5Tinsert(dtype_local, "real", 0, dtype_data) < 0) {
                ret = -1;
                goto end;
            }
            if (H5Tinsert(dtype_local, "imag", t, dtype_data) < 0) {
                ret = -1;
                goto end;
            }
            if (H5Tpack(dtype_local) < 0 ) {
                ret = -1;
                goto end;
            }
        } else {
            dtype_local = dtype_data;
        }

        if ( H5Lexists(val_group_id, "data", H5P_DEFAULT) > 0 ) {
            if ( H5Ldelete(val_group_id, "data", H5P_DEFAULT) < 0 ) {
                csc_error_message("Failed to remove previous entry %s/value/data\n", dset_name);
                ret = -1;
                goto end;
            }
        }

        dims[0] = nnz;
        dims[1] = 1;
        memspace_id = H5Screate_simple(2, dims, NULL);
        if ( memspace_id < 0 ) {
            csc_error_message("Failed to create memspace.\n");
            ret = -1;
            goto end;
        }

        dataset_id = H5Dcreate2(val_group_id, "data", dtype_local, memspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        if ( dataset_id < 0 ) {
            csc_error_message("Failed to create dataset %s/value/data\n", dset_name);
            ret = -1;
            goto end;
        }
        err = H5Dwrite(dataset_id, dtype_local, memspace_id, memspace_id, H5P_DEFAULT, data);
        if ( err < 0 ) {
            csc_error_message("Failed to write to %s/value/data\n", dset_name);
            ret = -1;
            goto end;
        }
        H5Dclose(dataset_id);
        H5Sclose(memspace_id);
        if ( dtype_local != dtype_data) H5Tclose(dtype_local);

        H5Gclose(val_group_id);
        H5Gclose(group_id);

        return 0;
    } else {
        // Matlab compatible
        unsigned long nr, nc, nz;

        nr = rows;
        nc = cols;
        nz = nnz;

        group_id = H5Gopen2(root, dset_name, H5P_DEFAULT);
        if ( group_id < 0 ) goto end;

        // CIDX Vector length cols+1
        if ( H5Lexists(group_id, "jc", H5P_DEFAULT) > 0 ) {
            if ( H5Ldelete(group_id, "jc", H5P_DEFAULT) < 0 ) {
                csc_error_message("Failed to remove previous entry %s/jc\n", dset_name);
                ret = -1;
                goto end;
            }
        }

        dims[0] = cols + 1;
        memspace_id = H5Screate_simple(1, dims, NULL);
        if ( memspace_id < 0 ) {
            csc_error_message("Failed to create memspace.\n");
            ret = -1;
            goto end;
        }

        dataset_id = H5Dcreate2(group_id, "jc", dtype_cols, memspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        if ( dataset_id < 0 ) {
            csc_error_message("Failed to create dataset %s/jc\n", dset_name);
            ret = -1;
            goto end;
        }

        err = H5Dwrite(dataset_id, dtype_cols, memspace_id, memspace_id, H5P_DEFAULT, colptr);
        if ( err < 0 ) {
            csc_error_message("Failed to write to %s/jc\n", dset_name);
            ret = -1;
            goto end;
        }
        H5Dclose(dataset_id);
        H5Sclose(memspace_id);

        // RIDX Vector length nnz
        if ( H5Lexists(group_id, "ir", H5P_DEFAULT) > 0 ) {
            if ( H5Ldelete(group_id, "ir", H5P_DEFAULT) < 0 ) {
                csc_error_message("Failed to remove previous entry %s/ir\n", dset_name);
                ret = -1;
                goto end;
            }
        }

        dims[0] = nnz;
        memspace_id = H5Screate_simple(1, dims, NULL);
        if ( memspace_id < 0 ) {
            csc_error_message("Failed to create memspace.\n");
            ret = -1;
            goto end;
        }

        dataset_id = H5Dcreate2(group_id, "ir", dtype_rows, memspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        if ( dataset_id < 0 ) {
            csc_error_message("Failed to create dataset %s/value/ir\n", dset_name);
            ret = -1;
            goto end;
        }
        err = H5Dwrite(dataset_id, dtype_rows, memspace_id, memspace_id, H5P_DEFAULT, rowptr);
        if ( err < 0 ) {
            csc_error_message("Failed to write to %s/value/ir\n", dset_name);
            ret = -1;
            goto end;
        }
        H5Dclose(dataset_id);
        H5Sclose(memspace_id);

        // data
        if ( cpx_stride > 1 ) {
            size_t t = H5Tget_size(dtype_data);
            dtype_local = H5Tcreate(H5T_COMPOUND, 2*t);
            if ( dtype_local < 0 ) {
                csc_error_message("Failed to create complex type");
                ret = -1;
                goto end;
            }
            if (H5Tinsert(dtype_local, "real", 0, dtype_data) < 0) {
                ret = -1;
                goto end;
            }
            if (H5Tinsert(dtype_local, "imag", t, dtype_data) < 0) {
                ret = -1;
                goto end;
            }
            if (H5Tpack(dtype_local) < 0 ) {
                ret = -1;
                goto end;
            }
        } else {
            dtype_local = dtype_data;
        }

        if ( H5Lexists(group_id, "data", H5P_DEFAULT) > 0 ) {
            if ( H5Ldelete(group_id, "data", H5P_DEFAULT) < 0 ) {
                csc_error_message("Failed to remove previous entry %s/data\n", dset_name);
                ret = -1;
                goto end;
            }
        }

        dims[0] = nnz;
        memspace_id = H5Screate_simple(1, dims, NULL);
        if ( memspace_id < 0 ) {
            csc_error_message("Failed to create memspace.\n");
            ret = -1;
            goto end;
        }

        dataset_id = H5Dcreate2(group_id, "data", dtype_local, memspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        if ( dataset_id < 0 ) {
            csc_error_message("Failed to create dataset %s/data\n", dset_name);
            ret = -1;
            goto end;
        }
        err = H5Dwrite(dataset_id, dtype_local, memspace_id, memspace_id, H5P_DEFAULT, data);
        if ( err < 0 ) {
            csc_error_message("Failed to write to %s/value/data\n", dset_name);
            ret = -1;
            goto end;
        }
        H5Dclose(dataset_id);
        H5Sclose(memspace_id);
        if ( dtype_local != dtype_data) H5Tclose(dtype_local);


        if ( csc_hdf5_attribute_write_ulong(root, dset_name, "ROWS", nr)) goto end;
        if ( csc_hdf5_attribute_write_ulong(root, dset_name, "COLS", nc)) goto end;
        if ( csc_hdf5_attribute_write_ulong(root, dset_name, "NNZ", nz)) goto end;


        // Write Matlab attributes
        if ( opts->matlab_attributes) {
            if ( dtype_data == H5T_NATIVE_FLOAT ) {
                if ( csc_hdf5_attribute_write_string(root, dset_name, "MATLAB_class", "single")) goto end;
            }
            if ( dtype_data == H5T_NATIVE_DOUBLE ) {
                if ( csc_hdf5_attribute_write_string(root, dset_name, "MATLAB_class", "double")) goto end;
            }
            if ( csc_hdf5_attribute_write_ulong(root, dset_name, "MATLAB_sparse", nr)) goto end;
        }

        H5Gclose(group_id);
        return 0;
    }

end:
    if ( dtype_local != dtype_data && dtype_local > 0) H5Tclose(dtype_local);
    if ( dataset_id > 0 ) H5Dclose(dataset_id);
    if ( memspace_id > 0 ) H5Sclose(memspace_id);
    if ( val_group_id > 0 ) H5Gclose(val_group_id);
    if ( group_id > 0 ) H5Gclose(group_id);
    return ret;

}


int csc_hdf5_write_double_sparse_matrix_int(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz,
                                            int * rowptr, int *colptr, double *data, csc_hdf5_options_t *opts)
{
    return csc_hdf5_write_sparse_ex(root, dset_name, rows, cols, nnz, H5T_NATIVE_INT, (void*) rowptr,
            H5T_NATIVE_INT, (void*) colptr, H5T_NATIVE_DOUBLE, (void *) data, 1, opts);
}

int csc_hdf5_write_double_sparse_matrix_uint(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz,
                                            unsigned int * rowptr, unsigned int *colptr, double *data, csc_hdf5_options_t *opts)
{
    return csc_hdf5_write_sparse_ex(root, dset_name, rows, cols, nnz, H5T_NATIVE_UINT, (void*) rowptr,
            H5T_NATIVE_UINT, (void*) colptr, H5T_NATIVE_DOUBLE, (void *) data, 1, opts);
}

int csc_hdf5_write_double_sparse_matrix_long(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz,
                                            long * rowptr, long *colptr, double *data, csc_hdf5_options_t *opts)
{
    return csc_hdf5_write_sparse_ex(root, dset_name, rows, cols, nnz, H5T_NATIVE_LONG, (void*) rowptr,
            H5T_NATIVE_LONG, (void*) colptr, H5T_NATIVE_DOUBLE, (void *) data, 1, opts);
}

int csc_hdf5_write_double_sparse_matrix_ulong(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz,
                                            unsigned long * rowptr, unsigned long *colptr, double *data, csc_hdf5_options_t *opts)
{
    return csc_hdf5_write_sparse_ex(root, dset_name, rows, cols, nnz, H5T_NATIVE_ULONG, (void*) rowptr,
            H5T_NATIVE_ULONG, (void*) colptr, H5T_NATIVE_DOUBLE, (void *) data, 1, opts);
}

int csc_hdf5_write_float_sparse_matrix_int(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz,
                                            int * rowptr, int *colptr, float *data, csc_hdf5_options_t *opts)
{
    return csc_hdf5_write_sparse_ex(root, dset_name, rows, cols, nnz, H5T_NATIVE_INT, (void*) rowptr,
            H5T_NATIVE_INT, (void*) colptr, H5T_NATIVE_FLOAT, (void *) data, 1, opts);
}

int csc_hdf5_write_float_sparse_matrix_uint(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz,
                                            unsigned int * rowptr, unsigned int *colptr, float *data, csc_hdf5_options_t *opts)
{
    return csc_hdf5_write_sparse_ex(root, dset_name, rows, cols, nnz, H5T_NATIVE_UINT, (void*) rowptr,
            H5T_NATIVE_UINT, (void*) colptr, H5T_NATIVE_FLOAT, (void *) data, 1, opts);
}

int csc_hdf5_write_float_sparse_matrix_long(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz,
                                            long * rowptr, long *colptr, float *data, csc_hdf5_options_t *opts)
{
    return csc_hdf5_write_sparse_ex(root, dset_name, rows, cols, nnz, H5T_NATIVE_LONG, (void*) rowptr,
            H5T_NATIVE_LONG, (void*) colptr, H5T_NATIVE_FLOAT, (void *) data, 1, opts);
}

int csc_hdf5_write_float_sparse_matrix_ulong(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz,
                                            unsigned long * rowptr, unsigned long *colptr, float *data, csc_hdf5_options_t *opts)
{
    return csc_hdf5_write_sparse_ex(root, dset_name, rows, cols, nnz, H5T_NATIVE_ULONG, (void*) rowptr,
            H5T_NATIVE_ULONG, (void*) colptr, H5T_NATIVE_FLOAT, (void *) data, 1, opts);
}

int csc_hdf5_write_double_complex_sparse_matrix_int(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz,
                                            int * rowptr, int *colptr, double complex *data, csc_hdf5_options_t *opts)
{
    return csc_hdf5_write_sparse_ex(root, dset_name, rows, cols, nnz, H5T_NATIVE_INT, (void*) rowptr,
            H5T_NATIVE_INT, (void*) colptr, H5T_NATIVE_DOUBLE, (void *) data, 2, opts);
}

int csc_hdf5_write_double_complex_sparse_matrix_uint(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz,
                                            unsigned int * rowptr, unsigned int *colptr, double complex *data, csc_hdf5_options_t *opts)
{
    return csc_hdf5_write_sparse_ex(root, dset_name, rows, cols, nnz, H5T_NATIVE_UINT, (void*) rowptr,
            H5T_NATIVE_UINT, (void*) colptr, H5T_NATIVE_DOUBLE, (void *) data, 2, opts);
}

int csc_hdf5_write_double_complex_sparse_matrix_long(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz,
                                            long * rowptr, long *colptr, double complex *data, csc_hdf5_options_t *opts)
{
    return csc_hdf5_write_sparse_ex(root, dset_name, rows, cols, nnz, H5T_NATIVE_LONG, (void*) rowptr,
            H5T_NATIVE_LONG, (void*) colptr, H5T_NATIVE_DOUBLE, (void *) data, 2, opts);
}

int csc_hdf5_write_double_complex_sparse_matrix_ulong(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz,
                                            unsigned long * rowptr, unsigned long *colptr, double complex *data, csc_hdf5_options_t *opts)
{
    return csc_hdf5_write_sparse_ex(root, dset_name, rows, cols, nnz, H5T_NATIVE_ULONG, (void*) rowptr,
            H5T_NATIVE_ULONG, (void*) colptr, H5T_NATIVE_DOUBLE, (void *) data, 2, opts);
}

int csc_hdf5_write_float_complex_sparse_matrix_int(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz,
                                            int * rowptr, int *colptr, float complex *data, csc_hdf5_options_t *opts)
{
    return csc_hdf5_write_sparse_ex(root, dset_name, rows, cols, nnz, H5T_NATIVE_INT, (void*) rowptr,
            H5T_NATIVE_INT, (void*) colptr, H5T_NATIVE_FLOAT, (void *) data, 2, opts);
}

int csc_hdf5_write_float_complex_sparse_matrix_uint(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz,
                                            unsigned int * rowptr, unsigned int *colptr, float complex *data, csc_hdf5_options_t *opts)
{
    return csc_hdf5_write_sparse_ex(root, dset_name, rows, cols, nnz, H5T_NATIVE_UINT, (void*) rowptr,
            H5T_NATIVE_UINT, (void*) colptr, H5T_NATIVE_FLOAT, (void *) data, 2, opts);
}

int csc_hdf5_write_float_complex_sparse_matrix_long(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz,
                                            long * rowptr, long *colptr, float complex *data, csc_hdf5_options_t *opts)
{
    return csc_hdf5_write_sparse_ex(root, dset_name, rows, cols, nnz, H5T_NATIVE_LONG, (void*) rowptr,
            H5T_NATIVE_LONG, (void*) colptr, H5T_NATIVE_FLOAT, (void *) data, 2, opts);
}

int csc_hdf5_write_float_complex_sparse_matrix_ulong(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz,
                                            unsigned long * rowptr, unsigned long *colptr, float complex *data, csc_hdf5_options_t *opts)
{
    return csc_hdf5_write_sparse_ex(root, dset_name, rows, cols, nnz, H5T_NATIVE_ULONG, (void*) rowptr,
            H5T_NATIVE_ULONG, (void*) colptr, H5T_NATIVE_FLOAT, (void *) data, 2, opts);
}


/*
 * Read functions
 *
 */
static int read_simple_vector(hid_t root, const char *dset_name, hid_t dtype, void *data)
{
    hid_t dataset_id = 0;
    hid_t datatype_id = 0;
    hid_t filespace_id = 0;
    hid_t memspace_id = 0;
    htri_t err = 0;
    int rank = 0;
    hsize_t read_dims[2];

    dataset_id = H5Dopen2(root, dset_name, H5P_DEFAULT);
    if ( dataset_id < 0) goto end;

    datatype_id = H5Dget_type(dataset_id);
    if ( datatype_id < 0 ) goto end;

    filespace_id = H5Dget_space(dataset_id);
    if ( filespace_id < 0 ) goto end;

    rank = H5Sget_simple_extent_ndims(filespace_id);
    if ( rank < 0 || rank > 2) goto end;

    err = H5Sget_simple_extent_dims(filespace_id, read_dims, NULL);
    if ( err < 0 ) goto end;

    if ( rank > 1 && read_dims[1] != 1 ) goto end;

    memspace_id = H5Screate_simple(1, read_dims, NULL);
    if ( memspace_id < 0 ) goto end;

    err = H5Dread(dataset_id, dtype, memspace_id, filespace_id, H5P_DEFAULT, data );
    if ( err < 0 ) goto end;

    H5Sclose(memspace_id);
    H5Sclose(filespace_id);
    H5Tclose(datatype_id);
    H5Dclose(dataset_id);
    return 0;
end:
    if ( memspace_id > 0 ) H5Sclose(memspace_id);
    if ( filespace_id > 0 ) H5Sclose(filespace_id);
    if ( dataset_id > 0 ) H5Tclose(datatype_id);
    if ( dataset_id > 0 ) H5Dclose(dataset_id);
    return -1;
}


static int csc_hdf5_read_sparse_ex(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        hid_t dtype_rows, void *rowptr,
        hid_t dtype_cols, void *colptr,
        hid_t dtype_data, void *data,
        int cpx_stride, csc_hdf5_options_t *opts)
{
    int ret = -1;
    char *type_str = NULL;
    hid_t group_id = 0;
    hid_t val_group_id = 0;
    hid_t dtype_local = 0;
    hid_t dataset_id = 0;
    hid_t space_id = 0;
    opts = opts;
    hsize_t dims[2];
    int rank;
    htri_t err;

    *rows = 0;
    *cols = 0;
    *nnz = 0;

    if ( ! csc_hdf5_group_exist(root, dset_name)) {
        return -1;
    }
    if ( ! csc_hdf5_is_sparse(root, dset_name)) {
        return -1;
    }

    if ( csc_hdf5_attribute_exist(root, dset_name, "OCTAVE_NEW_FORMAT") ) {
        // Octave
        size_t len = 0;
        unsigned long nr, nc, nz;

        group_id = H5Gopen2(root, dset_name, H5P_DEFAULT);
        if ( group_id < 0 ) goto end;

        if ( csc_hdf5_read_string(group_id, "type", &type_str, &len)) goto end;

        if ( strcmp(type_str, "sparse complex matrix" ) == 0 && cpx_stride == 1) goto end;

        val_group_id = H5Gopen2(group_id, "value", H5P_DEFAULT);
        if ( val_group_id < 0) goto end;

        // read rows/cols/nz
        if ( csc_hdf5_read_ulong(val_group_id, "nr", &nr)) { csc_error_message("failed to read %s/value/nr.", dset_name); goto end;}
        if ( csc_hdf5_read_ulong(val_group_id, "nc", &nc)) { csc_error_message("failed to read %s/value/nc.", dset_name); goto end;}
        if ( csc_hdf5_read_ulong(val_group_id, "nz", &nz)) { csc_error_message("failed to read %s/value/nz.", dset_name); goto end;}
        *rows = nr;
        *cols = nc;
        *nnz  = nz;

        // Read rowptr
        if ( read_simple_vector(val_group_id, "ridx", dtype_rows, rowptr)) {
            csc_error_message("Failed to read %s/value/ridx.", dset_name);
            ret = -1;
            goto end;
        }

        // Read colptr
        if ( read_simple_vector(val_group_id, "cidx", dtype_cols, colptr)) {
            csc_error_message("Failed to read %s/value/cidx.", dset_name);
            ret = -1;
            goto end;
        }

        if ( cpx_stride == 2) {
            size_t t = H5Tget_size(dtype_data);
            dtype_local = H5Tcreate(H5T_COMPOUND, 2*t);
            if ( dtype_local < 0 ) {
                csc_error_message("Failed to create complex type");
                goto end;
            }
            if (H5Tinsert(dtype_local, "real", 0, dtype_data) < 0) {
                goto end;
            }
            if (H5Tinsert(dtype_local, "imag", t, dtype_data) < 0) {
                goto end;
            }
            if (H5Tpack(dtype_local) < 0 ) {
                goto end;
            }
        } else {
            dtype_local = dtype_data;
        }

        if ( read_simple_vector(val_group_id, "data", dtype_local, data)) {
            csc_error_message("Failed to read %s/value/data.", dset_name);
            goto end;
        }


        if ( dtype_local > 0 && dtype_local != dtype_data) H5Tclose(dtype_local);
        free(type_str);
        H5Gclose(val_group_id);
        H5Gclose(group_id);
        return 0;
    } else {
        // Matlab and internal
        unsigned long nr, nc, nz;
        int matlab = 0;

        if ( csc_hdf5_attribute_exist(root, dset_name, "MATLAB_sparse")) {
            if ( csc_hdf5_attribute_read_ulong(root, dset_name, "MATLAB_sparse", &nr)) goto end;
            matlab = 1;
        } else if ( csc_hdf5_attribute_exist(root, dset_name, "ROWS")) {
            if ( csc_hdf5_attribute_read_ulong(root, dset_name, "ROWS", &nr)) goto end;
        } else {
            csc_error_message("Could not determine the number of rows in %s.", dset_name);
            goto end;
        }
        *rows = nr;

        group_id = H5Gopen2(root, dset_name, H5P_DEFAULT);
        if ( group_id < 0 ) goto end;

        if (matlab) {
            dataset_id = H5Dopen2(group_id, "jc", H5P_DEFAULT);
            if ( dataset_id < 0 ) goto end;

            space_id = H5Dget_space(dataset_id);
            if ( space_id < 0 ) goto end;

            rank = H5Sget_simple_extent_ndims(space_id);
            if ( rank < 0 || rank > 1) goto end;
            err = H5Sget_simple_extent_dims(space_id, dims, NULL);
            if ( err < 0 ) goto end;
            *cols = dims[0]-1;
            H5Sclose(space_id);
            space_id = 0;
            H5Dclose(dataset_id);
            dataset_id = 0;
        } else {
            if ( !csc_hdf5_attribute_exist(root, dset_name,"COLS")) goto end;
            if ( csc_hdf5_attribute_read_ulong(root, dset_name, "COLS", &nc)) goto end;
            *cols = nc;
        }

        if ( matlab) {
            dataset_id = H5Dopen2(group_id, "data", H5P_DEFAULT);
            if ( dataset_id < 0 ) goto end;

            space_id = H5Dget_space(dataset_id);
            if ( space_id < 0 ) goto end;

            rank = H5Sget_simple_extent_ndims(space_id);
            if ( rank < 0 || rank > 1) goto end;
            err = H5Sget_simple_extent_dims(space_id, dims, NULL);
            if ( err < 0 ) goto end;
            *nnz = dims[0];

            H5Sclose(space_id);
            space_id = 0;
            H5Dclose(dataset_id);
            dataset_id = 0;
        } else {
            if ( !csc_hdf5_attribute_exist(root, dset_name,"NNZ")) goto end;
            if ( csc_hdf5_attribute_read_ulong(root, dset_name, "NNZ", &nz)) goto end;
            *nnz = nz;
        }

        // Read rowptr
        if ( read_simple_vector(group_id, "ir", dtype_rows, rowptr)) {
            csc_error_message("Failed to read %s/ir.", dset_name);
            ret = -1;
            goto end;
        }

        // Read colptr
        if ( read_simple_vector(group_id, "jc", dtype_cols, colptr)) {
            csc_error_message("Failed to read %s/jc.", dset_name);
            ret = -1;
            goto end;
        }

        if ( cpx_stride == 2) {
            size_t t = H5Tget_size(dtype_data);
            dtype_local = H5Tcreate(H5T_COMPOUND, 2*t);
            if ( dtype_local < 0 ) {
                csc_error_message("Failed to create complex type");
                goto end;
            }
            if (H5Tinsert(dtype_local, "real", 0, dtype_data) < 0) {
                goto end;
            }
            if (H5Tinsert(dtype_local, "imag", t, dtype_data) < 0) {
                goto end;
            }
            if (H5Tpack(dtype_local) < 0 ) {
                goto end;
            }
        } else {
            dtype_local = dtype_data;
        }

        if ( read_simple_vector(group_id, "data", dtype_local, data)) {
            csc_error_message("Failed to read %s/data.", dset_name);
            goto end;
        }


        if ( dtype_local > 0 && dtype_local != dtype_data) H5Tclose(dtype_local);




        H5Gclose(group_id);


        return 0;
    }



end:

    if (space_id > 0 ) H5Sclose(space_id);
    if (dataset_id > 0) H5Dclose(dataset_id);
    if ( dtype_local > 0 && dtype_local != dtype_data) H5Tclose(dtype_local);
    if (val_group_id > 0 ) H5Gclose(val_group_id);
    if (group_id > 0 ) H5Gclose(group_id);
    if ( type_str ) free(type_str);
    return ret;
}

int csc_hdf5_read_double_sparse_matrix_int(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        int *rowptr, int *colptr, double * values, csc_hdf5_options_t *opts)
{
    return csc_hdf5_read_sparse_ex(root, dset_name, rows, cols, nnz,
            H5T_NATIVE_INT, (void *) rowptr,
            H5T_NATIVE_INT, (void *) colptr,
            H5T_NATIVE_DOUBLE, (void *) values, 1, opts);
}

int csc_hdf5_read_double_sparse_matrix_uint(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        unsigned int *rowptr, unsigned int *colptr, double * values, csc_hdf5_options_t *opts)
{
    return csc_hdf5_read_sparse_ex(root, dset_name, rows, cols, nnz,
            H5T_NATIVE_UINT, (void *) rowptr,
            H5T_NATIVE_UINT, (void *) colptr,
            H5T_NATIVE_DOUBLE, (void *) values, 1, opts);
}

int csc_hdf5_read_double_sparse_matrix_long(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        long *rowptr, long *colptr, double * values, csc_hdf5_options_t *opts)
{
    return csc_hdf5_read_sparse_ex(root, dset_name, rows, cols, nnz,
            H5T_NATIVE_LONG, (void *) rowptr,
            H5T_NATIVE_LONG, (void *) colptr,
            H5T_NATIVE_DOUBLE, (void *) values, 1, opts);
}

int csc_hdf5_read_double_sparse_matrix_ulong(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        unsigned long *rowptr, unsigned long *colptr, double * values, csc_hdf5_options_t *opts)
{
    return csc_hdf5_read_sparse_ex(root, dset_name, rows, cols, nnz,
            H5T_NATIVE_ULONG, (void *) rowptr,
            H5T_NATIVE_ULONG, (void *) colptr,
            H5T_NATIVE_DOUBLE, (void *) values, 1, opts);
}

int csc_hdf5_read_float_sparse_matrix_int(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        int *rowptr, int *colptr, float * values, csc_hdf5_options_t *opts)
{
    return csc_hdf5_read_sparse_ex(root, dset_name, rows, cols, nnz,
            H5T_NATIVE_INT, (void *) rowptr,
            H5T_NATIVE_INT, (void *) colptr,
            H5T_NATIVE_FLOAT, (void *) values, 1, opts);
}

int csc_hdf5_read_float_sparse_matrix_uint(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        unsigned int *rowptr, unsigned int *colptr, float * values, csc_hdf5_options_t *opts)
{
    return csc_hdf5_read_sparse_ex(root, dset_name, rows, cols, nnz,
            H5T_NATIVE_UINT, (void *) rowptr,
            H5T_NATIVE_UINT, (void *) colptr,
            H5T_NATIVE_FLOAT, (void *) values, 1, opts);
}

int csc_hdf5_read_float_sparse_matrix_long(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        long *rowptr, long *colptr, float * values, csc_hdf5_options_t *opts)
{
    return csc_hdf5_read_sparse_ex(root, dset_name, rows, cols, nnz,
            H5T_NATIVE_LONG, (void *) rowptr,
            H5T_NATIVE_LONG, (void *) colptr,
            H5T_NATIVE_FLOAT, (void *) values, 1, opts);
}

int csc_hdf5_read_float_sparse_matrix_ulong(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        unsigned long *rowptr, unsigned long *colptr, float * values, csc_hdf5_options_t *opts)
{
    return csc_hdf5_read_sparse_ex(root, dset_name, rows, cols, nnz,
            H5T_NATIVE_ULONG, (void *) rowptr,
            H5T_NATIVE_ULONG, (void *) colptr,
            H5T_NATIVE_FLOAT, (void *) values, 1, opts);
}


int csc_hdf5_read_double_complex_sparse_matrix_int(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        int *rowptr, int *colptr, double complex * values, csc_hdf5_options_t *opts)
{
    return csc_hdf5_read_sparse_ex(root, dset_name, rows, cols, nnz,
            H5T_NATIVE_INT, (void *) rowptr,
            H5T_NATIVE_INT, (void *) colptr,
            H5T_NATIVE_DOUBLE, (void *) values, 2, opts);
}

int csc_hdf5_read_double_complex_sparse_matrix_uint(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        unsigned int *rowptr, unsigned int *colptr, double complex * values, csc_hdf5_options_t *opts)
{
    return csc_hdf5_read_sparse_ex(root, dset_name, rows, cols, nnz,
            H5T_NATIVE_UINT, (void *) rowptr,
            H5T_NATIVE_UINT, (void *) colptr,
            H5T_NATIVE_DOUBLE, (void *) values, 2, opts);
}

int csc_hdf5_read_double_complex_sparse_matrix_long(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        long *rowptr, long *colptr, double complex * values, csc_hdf5_options_t *opts)
{
    return csc_hdf5_read_sparse_ex(root, dset_name, rows, cols, nnz,
            H5T_NATIVE_LONG, (void *) rowptr,
            H5T_NATIVE_LONG, (void *) colptr,
            H5T_NATIVE_DOUBLE, (void *) values, 2, opts);
}

int csc_hdf5_read_double_complex_sparse_matrix_ulong(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        unsigned long *rowptr, unsigned long *colptr, double complex * values, csc_hdf5_options_t *opts)
{
    return csc_hdf5_read_sparse_ex(root, dset_name, rows, cols, nnz,
            H5T_NATIVE_ULONG, (void *) rowptr,
            H5T_NATIVE_ULONG, (void *) colptr,
            H5T_NATIVE_DOUBLE, (void *) values, 2, opts);
}

int csc_hdf5_read_float_complex_sparse_matrix_int(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        int *rowptr, int *colptr, float complex * values, csc_hdf5_options_t *opts)
{
    return csc_hdf5_read_sparse_ex(root, dset_name, rows, cols, nnz,
            H5T_NATIVE_INT, (void *) rowptr,
            H5T_NATIVE_INT, (void *) colptr,
            H5T_NATIVE_FLOAT, (void *) values, 2, opts);
}

int csc_hdf5_read_float_complex_sparse_matrix_uint(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        unsigned int *rowptr, unsigned int *colptr, float complex * values, csc_hdf5_options_t *opts)
{
    return csc_hdf5_read_sparse_ex(root, dset_name, rows, cols, nnz,
            H5T_NATIVE_UINT, (void *) rowptr,
            H5T_NATIVE_UINT, (void *) colptr,
            H5T_NATIVE_FLOAT, (void *) values, 2, opts);
}

int csc_hdf5_read_float_complex_sparse_matrix_long(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        long *rowptr, long *colptr, float complex * values, csc_hdf5_options_t *opts)
{
    return csc_hdf5_read_sparse_ex(root, dset_name, rows, cols, nnz,
            H5T_NATIVE_LONG, (void *) rowptr,
            H5T_NATIVE_LONG, (void *) colptr,
            H5T_NATIVE_FLOAT, (void *) values, 2, opts);
}

int csc_hdf5_read_float_complex_sparse_matrix_ulong(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        unsigned long *rowptr, unsigned long *colptr, float complex * values, csc_hdf5_options_t *opts)
{
    return csc_hdf5_read_sparse_ex(root, dset_name, rows, cols, nnz,
            H5T_NATIVE_ULONG, (void *) rowptr,
            H5T_NATIVE_ULONG, (void *) colptr,
            H5T_NATIVE_FLOAT, (void *) values, 2, opts);
}





/*
 *
 * Old routines
 *
 *
 */

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


