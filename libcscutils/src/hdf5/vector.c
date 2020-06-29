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


/* Write a vector to a data set   */
int csc_hdf5_vector_write(csc_hdf5_field_t field, hid_t root, const char *dset_name, size_t len, void *vector)
{
    herr_t err;
    hid_t vg;
    hsize_t dims[1] = { (hsize_t) len };
    unsigned long tmp;

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
    /*   Set the attributes  */
    err = csc_hdf5_set_content(root, dset_name, CSC_HDF5_VECTOR);
    if ( err < 0 ) {
        H5Gclose(vg);
        csc_error_message("Failed to set the content type.\n");
        return -1;
    }

    err = csc_hdf5_set_field(root,dset_name,field);
    if ( err < 0 ) {
        csc_error_message("Failed to set the field type.\n");
        H5Gclose(vg);
        return -1;
    }

    tmp = len;
    err = H5LTset_attribute_ulong(root, dset_name, "len", &tmp, 1);
    if ( err < 0 ) {
        csc_error_message("Failed to set attribute rows.\n");
        H5Gclose(vg);
        return -1;
    }

    if ( field == CSC_HDF5_REAL ) {
        err = csc_hdf5_create_dataset(vg, "values", 1, dims, H5T_NATIVE_DOUBLE, vector);
    } else if ( field == CSC_HDF5_COMPLEX) {
        hid_t complex_type = csc_hdf5_complex_type(root);
        if ( complex_type < 0 ) {
            csc_error_message("Failed to create complex type.");
            H5Gclose(vg);
            return -1;
        }
        err = csc_hdf5_create_dataset(vg, "values", 1, dims, complex_type, vector);
        H5Tclose(complex_type);
    } else if ( field == CSC_HDF5_INTEGER8 ) {
        err = csc_hdf5_create_dataset(vg, "values", 1, dims, H5T_NATIVE_CHAR, vector);
    } else if ( field == CSC_HDF5_INTEGER16 ) {
        err = csc_hdf5_create_dataset(vg, "values", 1, dims, H5T_NATIVE_SHORT, vector);
    } else if ( field == CSC_HDF5_INTEGER32 ) {
        err = csc_hdf5_create_dataset(vg, "values", 1, dims, H5T_NATIVE_INT, vector);
    } else if ( field == CSC_HDF5_INTEGER64 ) {
        err = csc_hdf5_create_dataset(vg, "values", 1, dims, H5T_NATIVE_LONG, vector);
    }

    if ( err < 0 ) {
        csc_error_message("Failed to write a vector of length %lu to %s\n", (unsigned long) len, dset_name);
        H5Gclose(vg);
        return -1;
    }
    H5Gclose(vg);
    return 0;
}


long csc_hdf5_vector_len(hid_t root, const char *dset_name)
{
    size_t slen;

    if (! csc_hdf5_is_vector(root, dset_name)){
        csc_error_message("Dataset %s is not a vector.\n", dset_name);
        return -1;
    }
    HDF5_GET_ATTR_ULONG(root,dset_name,"len",slen);
    return (long) slen;
}

csc_hdf5_field_t csc_hdf5_vector_field(hid_t root, const char *dset_name)
{
    int err;
    csc_hdf5_field_t field;
    if (! csc_hdf5_is_vector(root, dset_name)){
        csc_error_message("Dataset %s is not a vector.\n", dset_name);
        return CSC_HDF5_UNKOWN_FIELD;
    }
    err = csc_hdf5_get_field(root,dset_name, &field);
    if ( err < 0 )
        return CSC_HDF5_UNKOWN_FIELD;

    return field;
}


int csc_hdf5_vector_read(csc_hdf5_field_t field, hid_t root, const char *dset_name, void * vector)
{
    herr_t err;
    hid_t vg;
    hid_t did;
    hid_t tid, tid_out;
    ssize_t elemsize, elemsize_out;
    ssize_t len;
    void *tmp;

    if (! csc_hdf5_is_vector(root, dset_name)){
        csc_error_message("Dataset %s is not a vector.\n", dset_name);
        return -1;
    }

    vg = H5Gopen2(root, dset_name, H5P_DEFAULT);
    if ( vg < 0 ) {
        csc_error_message("Failed to open vector %s\n", dset_name);
        return -1;
    }

    err = 0;
    if ( field == CSC_HDF5_INTEGER8
            || field == CSC_HDF5_INTEGER16
            || field == CSC_HDF5_INTEGER32
            || field == CSC_HDF5_INTEGER64 )
    {
        HDF5_GET_ATTR_ULONG(root,dset_name,"len",len);
        switch(field) {
            case CSC_HDF5_INTEGER8:
                elemsize_out = sizeof(char);
                tid_out = H5T_NATIVE_CHAR;
                break;
            case CSC_HDF5_INTEGER16:
                elemsize_out = sizeof(short);
                tid_out = H5T_NATIVE_SHORT;
                break;
            case CSC_HDF5_INTEGER32:
                elemsize_out = sizeof(int);
                tid_out = H5T_NATIVE_INT;
                break;
            case CSC_HDF5_INTEGER64:
                elemsize_out = sizeof(long);
                tid_out = H5T_NATIVE_LONG;
                break;
            default:
                elemsize_out = sizeof(long);
                tid_out = H5T_NATIVE_LONG;
        }


        if((did = H5Dopen2(vg, "values",H5P_DEFAULT)) < 0) {
            H5Gclose(vg);
            csc_error_message("Failed to open dataset %s\n", dset_name);
            return -1;
        }
        tid = H5Dget_type(did);
        elemsize = H5Tget_size(tid);
        elemsize = (elemsize > elemsize_out ) ? elemsize: elemsize_out;
        tmp = malloc(elemsize * len);
        if ( tmp == NULL ) {
            csc_error_message ("Failed to allocate temporary storage.\n");
            H5Tclose(tid);
            H5Dclose(did);
            H5Gclose(vg);
            return -1;
        }
        if(H5Dread(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, tmp) < 0){
            csc_error_message("failed to read values from %s\n", dset_name);
            H5Tclose(tid);
            H5Dclose(did);
            H5Gclose(vg);
            free(tmp);
            return -1;
        }
        if ( H5Tconvert(tid, tid_out, len, tmp, NULL, H5P_DEFAULT)< 0) {
            csc_error_message("failed to convert values from %s\n", dset_name);
            H5Tclose(tid);
            H5Dclose(did);
            H5Gclose(vg);
            free(tmp);
            return -1;
        }
        memcpy(vector, tmp, elemsize_out * len);
        H5Tclose(tid);
        H5Dclose(did);
        free(tmp);
        err = 0;
    } else if ( field == CSC_HDF5_REAL ) {
        err = H5LTread_dataset(vg, "values", H5T_NATIVE_DOUBLE, vector);

    } else if ( field == CSC_HDF5_COMPLEX ) {
        hid_t complex_type = csc_hdf5_complex_type(root);

        if ( complex_type < 0 ) {
            csc_error_message("Failed to create complex type.");
            H5Gclose(vg);
            return -1;
        }
        err = H5LTread_dataset(vg, "values", complex_type, vector);
        H5Tclose(complex_type);

    }

    if ( err < 0) {
        csc_error_message("Failed to read dataset %s.\n", dset_name);
        H5Gclose(vg);
        return -1;
    }

    H5Gclose(vg);
    return 0;
}

