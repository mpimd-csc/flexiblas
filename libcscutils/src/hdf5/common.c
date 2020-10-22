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
#include <time.h>
#include <sys/time.h>

#define H5_NO_DEPRECATED_SYMBOLS
#include <hdf5.h>
#include <hdf5_hl.h>

#include "cscutils/hdf.h"
#include "cscutils/strutils.h"
#include "csc_hdf5_common.h"
#include "filters/filter_bzip2.h"
#include "filters/filter_xz.h"
#include "filters/filter_zstd.h"


#define MIN(A,B) (((A)<(B))?(A):(B))

#define COMPRESSS_LEVEL 6

int CSC_HDF5_COMPRESSION = CSC_HDF5_COMPRESS_NONE;




int csc_hdf5_register_filters()
{
    int ret;
    if (H5Zfilter_avail(H5Z_FILTER_BZIP2) == 0 ) {
        ret = csc_hdf5_register_bzip2();
        /* printf("CSC_BZIP2_HDF5FILTER = %d\n", ret); */
    }
    if (H5Zfilter_avail(H5Z_FILTER_XZ) == 0 ) {
        ret = csc_hdf5_register_xz();
        /* printf("CSC_XZ_HDF5FILTER = %d\n", ret); */
    }
    if (H5Zfilter_avail(H5Z_FILTER_ZSTD) == 0 ) {
        ret = csc_hdf5_register_zstd();
        /* printf("CSC_XZ_HDF5FILTER = %d\n", ret); */
    }

    return ret;

}


void csc_hdf5_set_compression(csc_hdf5_compression_t comp)
{
    csc_hdf5_register_filters();
    CSC_HDF5_COMPRESSION = comp;
    return;
}

int csc_hdf5_has_compression(csc_hdf5_compression_t comp) {
    csc_hdf5_register_filters();
    switch(comp) {
        case CSC_HDF5_COMPRESS_NONE:
            return 1;
        case CSC_HDF5_COMPRESS_DEFLATE:
            if (H5Zfilter_avail(H5Z_FILTER_DEFLATE) > 0 )
                return 1;
            break;
        case CSC_HDF5_COMPRESS_BZIP2:
            if (H5Zfilter_avail(H5Z_FILTER_BZIP2) > 0 )
                return 1;
            break;
        case CSC_HDF5_COMPRESS_XZ:
            if (H5Zfilter_avail(H5Z_FILTER_XZ) > 0 )
                return 1;
            break;
        case CSC_HDF5_COMPRESS_ZSTD:
            if (H5Zfilter_avail(H5Z_FILTER_ZSTD) > 0 )
                return 1;
            break;
        default:
            return 0;
    }
    return 0;
}

csc_hdf5_compression_t csc_hdf5_get_compression()
{
    return CSC_HDF5_COMPRESSION;
}

int  csc_hdf5_get_compression_int()
{
    return (int) CSC_HDF5_COMPRESSION;
}

#define COMPRESS_MIN 1*1024*1024
int csc_hdf5_create_dataset(hid_t loc, const char *dset_name, int rank, const hsize_t *dims, hid_t tid, const void *data)
{
    hid_t dataspace = -1;
    hid_t dataset = -1;
    hid_t plist = -1;
    hid_t dplist = -1;
    hsize_t *cdims = NULL ;
    int i;
    size_t nslots, nbytes;
    double w0;
    herr_t status;
    hsize_t k, towrite;

    if ( (dataset = H5Screate_simple(rank, dims, NULL)) < 0 ) {
        goto fail;
    }

    plist = H5Pcreate(H5P_DATASET_CREATE);
    if ( plist < 0 ) {
        goto fail;
    }
    dplist = H5Pcreate(H5P_DATASET_ACCESS);
    if ( dplist < 0 ) {
        goto fail;
    }

    size_t ts= H5Tget_size(tid);
    towrite = 1;
    for (k = 0; k < (hsize_t) rank; k++) {
       towrite *= dims[k];
    }
    towrite *= ts;
    /* Enable compression  */
    if (CSC_HDF5_COMPRESSION != CSC_HDF5_COMPRESS_NONE && towrite >= COMPRESS_MIN) {
        /*  Adjust the junk size  */
        H5Pget_chunk_cache(dplist, &nslots, &nbytes, &w0);
        /*  Set cache size to 32MBs and instruct the cache to discard the fully read chunk */
        /* printf("Nbytes %lu\n", nbytes); */
        nbytes = 32*1024*1024;
        w0 = 1;
        H5Pset_chunk_cache(dplist, nslots, nbytes, w0);

        cdims = malloc(sizeof(hsize_t) * rank);
        for (i = 0; i < rank; i++) {
            cdims[i] = MIN(dims[i], 1024);
        }
        if ( H5Pset_chunk(plist, rank, cdims) < 0 ){
            free(cdims); cdims = NULL;
            goto fail;
        }
        free(cdims); cdims = NULL;

        /* Set Compression   */
        if ( CSC_HDF5_COMPRESSION == CSC_HDF5_COMPRESS_DEFLATE) {
            if ( H5Pset_deflate(plist, 9) < 0 ) {
                goto fail;
            }
        } else if ( CSC_HDF5_COMPRESSION == CSC_HDF5_COMPRESS_BZIP2 ) {
            htri_t avail = H5Zfilter_avail(H5Z_FILTER_BZIP2);
            if (!avail) {
                csc_error_message("bzip2 compression is not available.\n");
                goto fail;
            }
            status = H5Pset_filter (plist, (H5Z_filter_t)H5Z_FILTER_BZIP2, H5Z_FLAG_MANDATORY, (size_t)0, NULL);
            if (status < 0 ) {
                csc_error_message("Failed to set bzip2 compression.\n");
                goto fail;
            }
        } else if ( CSC_HDF5_COMPRESSION == CSC_HDF5_COMPRESS_XZ ) {
            htri_t avail = H5Zfilter_avail(H5Z_FILTER_XZ);
            if (!avail) {
                csc_error_message("xz compression is not available.\n");
                goto fail;
            }
            status = H5Pset_filter (plist, (H5Z_filter_t)H5Z_FILTER_XZ, H5Z_FLAG_MANDATORY, (size_t)0, NULL);
            if (status < 0 ) {
                csc_error_message("Failed to set xz compression.\n");
                goto fail;
            }
        } else if ( CSC_HDF5_COMPRESSION == CSC_HDF5_COMPRESS_ZSTD ) {
            htri_t avail = H5Zfilter_avail(H5Z_FILTER_ZSTD);
            if (!avail) {
                csc_error_message("zstd compression is not available.\n");
                goto fail;
            }
            status = H5Pset_filter (plist, (H5Z_filter_t)H5Z_FILTER_ZSTD, H5Z_FLAG_MANDATORY, (size_t)0, NULL);
            if (status < 0 ) {
                csc_error_message("Failed to set zstd compression.\n");
                goto fail;
            }
        }

    }
    dataspace = H5Dcreate2(loc, dset_name, tid, dataset, H5P_DEFAULT, plist, dplist);
    if ( dataspace < 0 ) {
        goto fail;
    }

    /* If data exists  */
    if ( data ) {
        if ( H5Dwrite(dataspace, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0 ) {
            goto fail;
        }
    }

    if ( H5Dclose(dataspace) < 0 ) {
        return -1;
    }
    if ( H5Pclose(plist) < 0 ) {
        return -1;
    }
    if ( H5Sclose(dataset) < 0 ) {
        return -1;
    }
    return 0;

fail:
    if (plist >= 0)  H5Pclose(plist);
    if (dplist >= 0) H5Pclose(dplist);
    if (dataspace >= 0) H5Sclose(dataspace);
    if (dataset >= 0) H5Dclose(dataset);
    return -1;
}




static herr_t csc_hdf5_set_attribute_generic( hid_t loc_id,  const char *obj_name, const char *attr_name, size_t size, hid_t tid, hid_t tid_file, const void *data )
{
    hid_t      obj_id, sid, attr_id;
    hsize_t    dim_size = size;
    int        has_attr = 0;

    /* check the arguments */
    if (obj_name == NULL)
        return -1;
    if (attr_name == NULL)
        return -1;

    /* Open the object */
    if ((obj_id = H5Oopen(loc_id, obj_name, H5P_DEFAULT)) < 0)
        return -1;

    /* Create the data space for the attribute. */
    if ( (sid = H5Screate_simple( 1, &dim_size, NULL )) < 0 )
        goto out;

    /* Verify if the attribute already exists */
    if ( csc_hdf5_attribute_exist(loc_id, obj_name, attr_name)) {
        has_attr = 1;
    }

    /* The attribute already exists, delete it */
    if(has_attr == 1) {
        if(H5Adelete(obj_id, attr_name) < 0)
            goto out;
    }
        /* Create the attribute. */
    if((attr_id = H5Acreate2(obj_id, attr_name, tid_file, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;

    /* Write the attribute data. */
    if(H5Awrite(attr_id, tid, data) < 0)
        goto out;

    /* Close the attribute. */
    if(H5Aclose(attr_id) < 0)
        goto out;

    /* Close the dataspace. */
    if(H5Sclose(sid) < 0)
        goto out;

    /* Close the object */
    if(H5Oclose(obj_id) < 0)
        return -1;

    return 0;

out:
    H5Oclose(obj_id);
    return -1;
}



/* Set the content attribute  */
int csc_hdf5_set_content(hid_t root, const char *dset_name, csc_hdf5_content_t content)
{
    hid_t content_type;
    hid_t content_type_file;
    herr_t err;

    err = csc_hdf5_content_type(root, &content_type, &content_type_file);
    if (err != 0 ) {
        csc_error_message("Failed to create Datatype\n");
        return -1;
    }
    int val = content;

    err = csc_hdf5_set_attribute_generic(root, dset_name, "csc_hdf5_content", 1, content_type, content_type_file, &val);
    if ( err < 0 ) {
        csc_error_message("Failed to write content attribute.");
        H5Tclose(content_type);
        H5Tclose(content_type_file);
        return -1;
    }
    H5Tclose(content_type);
    H5Tclose(content_type_file);
    return 0;
}

int csc_hdf5_get_content(hid_t root, const char *dset_name, csc_hdf5_content_t *content)
{
    hid_t content_type;
    hid_t content_type_file;
    int val = 0;
    herr_t err;

    if ( !csc_hdf5_attribute_exist(root,dset_name,"csc_hdf5_content") ) {
        return -1;
    }
    err = csc_hdf5_content_type(root, &content_type, &content_type_file);
    if (err != 0  ) {
        csc_error_message("Failed to create Datatype\n");
        return -1;
    }

    err = csc_hdf5_attribute_read_int(root, dset_name, "csc_hdf5_content", &val);
    if ( err < 0 ) {
        csc_error_message("Failed to get content.\n");
        H5Tclose(content_type);
        H5Tclose(content_type_file);
        return -1;
    }
    *content = (csc_hdf5_content_t) val;
    H5Tclose(content_type);
    H5Tclose(content_type_file);
    return 0;
}

/* Set the field attribute  */
int csc_hdf5_set_field(hid_t root, const char *dset_name, csc_hdf5_field_t field)
{
    hid_t field_type;
    hid_t field_type_file;

    herr_t err;
    err = csc_hdf5_field_type(root, &field_type, &field_type_file);
    if ( err < 0 ) {
        csc_error_message("Failed to create Datatype\n");
        return -1;
    }
    int val = field;
    err = csc_hdf5_set_attribute_generic(root, dset_name, "csc_hdf5_field", 1, field_type, field_type_file,  &val);
    if ( err < 0 ) {
        csc_error_message("Failed to write field attribute.");
        H5Tclose(field_type);
        H5Tclose(field_type_file);
        return -1;
    }
    H5Tclose(field_type);
    H5Tclose(field_type_file);
    return 0;
}

int csc_hdf5_get_field(hid_t root, const char *dset_name, csc_hdf5_field_t *field)
{
    hid_t field_type;
    hid_t field_type_file;
    int val = 0;
    herr_t err;

    if ( !csc_hdf5_attribute_exist(root,dset_name,"csc_hdf5_field") ) {
        return -1;
    }

    err = csc_hdf5_field_type(root, &field_type, &field_type_file);
    if (err < 0 ) {
        csc_error_message("Failed to create datatype\n");
        return -1;
    }

    err = csc_hdf5_attribute_read_int(root, dset_name, "csc_hdf5_field", &val);
    if ( err < 0 ) {
        csc_error_message("Failed to get field.\n");
        H5Tclose(field_type);
        H5Tclose(field_type_file);
        return -1;
    }
    H5Tclose(field_type);
    H5Tclose(field_type_file);

    *field = (csc_hdf5_field_t ) val;
    return 0;
}


/* Human readable strings of the H5T Class   */
char *csc_hdf5_get_classname(hid_t d) {
    switch (H5Tget_class(d)) {
        case H5T_INTEGER:
            return "H5T_INTEGER";
        case H5T_FLOAT:
            return "H5T_FLOAT";
        case H5T_STRING:
            return "H5T_STRING";
        case H5T_BITFIELD:
            return "H5T_BITFIELD";
        case H5T_OPAQUE:
            return "H5T_OPAQUE";
        case H5T_COMPOUND:
            return "H5T_COMPOUND";
        case H5T_REFERENCE:
            return "H5T_REFERENCE";
        case H5T_ENUM:
            return "H5T_ENUM";
        case H5T_VLEN:
            return "H5T_VLEN";
        case H5T_ARRAY:
            return "H5T_ARRAY";
        default:
            return "H5T_NO_CLASS";

    }
    return "H5T_NO_CLASS";
}

/* Check if the given dataset is a vector.  */
int csc_hdf5_is_vector(hid_t root, const char *dset_name)
{
    csc_hdf5_content_t content;
    if ( csc_hdf5_get_content(root, dset_name, &content) == 0 ) {
        if (content == CSC_HDF5_VECTOR )
            return 1;
        else
            return 0;
    } else {
        return 0;
    }
    return 0;
}


/* Check if the given dataset is a sparse csr  matrix.  */
int csc_hdf5_is_spcsr(hid_t root, const char *dset_name)
{
    csc_hdf5_content_t content;
    if ( csc_hdf5_get_content(root, dset_name, &content) == 0 ) {
        if (content == CSC_HDF5_SPARSE_CSR )
            return 1;
        else
            return 0;
    } else {
        return 0;
    }
    return 0;

}

/* Check if the given dataset is a sparse csc  matrix.  */
int csc_hdf5_is_spcsc(hid_t root, const char *dset_name)
{
    csc_hdf5_content_t content;
    if ( csc_hdf5_get_content(root, dset_name, &content) == 0 ) {
        if (content == CSC_HDF5_SPARSE_CSC )
            return 1;
        else
            return 0;
    } else {
        return 0;
    }
    return 0;
}

/* Check if the given dataset is a sparse coo  matrix.  */
int csc_hdf5_is_spcoo(hid_t root, const char *dset_name)
{
    csc_hdf5_content_t content;
    if ( csc_hdf5_get_content(root, dset_name, &content) == 0 ) {
        if (content == CSC_HDF5_SPARSE_COORDINATE )
            return 1;
        else
            return 0;
    } else {
        return 0;
    }
    return 0;
}

/* Real storage */
int csc_hdf5_is_real(hid_t root, const char *dset_name)
{
    csc_hdf5_field_t field;
    if ( csc_hdf5_get_field(root, dset_name, &field) == 0 ) {
        if (field == CSC_HDF5_REAL )
            return 1;
        else
            return 0;
    } else {
        return 0;
    }
    return 0;

}

/* Real storage */
int csc_hdf5_is_real_single(hid_t root, const char *dset_name)
{
    csc_hdf5_field_t field;
    if ( csc_hdf5_get_field(root, dset_name, &field) == 0 ) {
        if (field == CSC_HDF5_REAL_SINGLE )
            return 1;
        else
            return 0;
    } else {
        return 0;
    }
    return 0;

}


/* Complex storage */
int csc_hdf5_is_complex(hid_t root, const char *dset_name)
{
    csc_hdf5_field_t field;
    if ( csc_hdf5_get_field(root, dset_name, &field) == 0 ) {
        if (field == CSC_HDF5_COMPLEX )
            return 1;
        else
            return 0;
    } else {
        return 0;
    }
    return 0;
}

void csc_hdf5_print_native_type(hid_t dtype_id, H5T_direction_t direction )
{
    hid_t native_id;

    native_id = H5Tget_native_type(dtype_id, direction);
    if ( native_id < 0 ) {
        csc_error_message("Can not get the native type.\n");
        return;
    }


    printf("Native Type ( class = %s): ", csc_hdf5_get_classname(dtype_id));
    if ( H5Tequal(native_id,H5T_NATIVE_CHAR) > 0 ) {
        printf("H5T_NATIVE_CHAR");
    }
    if ( H5Tequal(native_id,H5T_NATIVE_SHORT) > 0 ) {
        printf("H5T_NATIVE_SHORT");
    }
    if ( H5Tequal(native_id,H5T_NATIVE_INT) > 0 ) {
        printf("H5T_NATIVE_INT");
    }
    if ( H5Tequal(native_id,H5T_NATIVE_LONG) > 0 ) {
        printf("H5T_NATIVE_LONG");
    }
    if ( H5Tequal(native_id,H5T_NATIVE_LLONG) > 0 ) {
        printf("H5T_NATIVE_LLONG");
    }

    if ( H5Tequal(native_id,H5T_NATIVE_UCHAR) > 0 ) {
        printf("H5T_NATIVE_UCHAR");
    }
    if ( H5Tequal(native_id,H5T_NATIVE_USHORT) > 0 ) {
        printf("H5T_NATIVE_USHORT");
    }
    if ( H5Tequal(native_id,H5T_NATIVE_UINT) > 0 ) {
        printf("H5T_NATIVE_UINT");
    }
    if ( H5Tequal(native_id,H5T_NATIVE_ULONG) > 0 ) {
        printf("H5T_NATIVE_ULONG");
    }
    if ( H5Tequal(native_id,H5T_NATIVE_ULLONG) > 0 ) {
        printf("H5T_NATIVE_ULLONG");
    }

    if ( H5Tequal(native_id,H5T_NATIVE_FLOAT) > 0 ) {
        printf("H5T_NATIVE_FLOAT");
    }
    if ( H5Tequal(native_id,H5T_NATIVE_DOUBLE) > 0 ) {
        printf("H5T_NATIVE_DOUBLE");
    }
    if ( H5Tequal(native_id,H5T_NATIVE_LDOUBLE) > 0 ) {
        printf("H5T_NATIVE_LDOUBLE");
    }

    if ( H5Tequal(native_id,H5T_NATIVE_B8) > 0 ) {
        printf("H5T_NATIVE_B8");
    }
    if ( H5Tequal(native_id,H5T_NATIVE_B16) > 0 ) {
        printf("H5T_NATIVE_B16");
    }
    if ( H5Tequal(native_id,H5T_NATIVE_B32) > 0 ) {
        printf("H5T_NATIVE_B32");
    }
    if ( H5Tequal(native_id,H5T_NATIVE_B64) > 0 ) {
        printf("H5T_NATIVE_B64");
    }
    printf("\n");
    H5Tclose(native_id);
    return;
}



hid_t csc_hdf5_group_close(hid_t group) {
    return H5Gclose(group);
}

hid_t csc_hdf5_group_open(hid_t root, const char *dset_name)
{
    hid_t group;
    if ( csc_hdf5_group_exist(root, dset_name) > 0 ) {
        group = H5Gopen2(root, dset_name, H5P_DEFAULT);
    } else {
        group = H5Gcreate2(root, dset_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    }
    return group;
}



int csc_hdf5_exist(hid_t h5file, const char *name)
{
    H5E_auto2_t func;
    void *data;
    hid_t status;
    int e = 0;
    int err;

    err = H5Eget_auto2( H5E_DEFAULT, &func, &data);
    H5Eset_auto2( H5E_DEFAULT, NULL, NULL);

    status = H5Oopen(h5file, name, H5P_DEFAULT);
    if ( status >= 0 ) {
        e = 1;
        H5Oclose(status);
    } else {
        e = 0;
    }
    if ( err >= 0) {
        H5Eset_auto2(H5E_DEFAULT, func, data);
    }
    return e;
}

int csc_hdf5_is_octave(hid_t root, const char *dset_name)
{
    if ( csc_hdf5_attribute_exist(root, dset_name, "OCTAVE_NEW_FORMAT")) {
        int val;
        if (csc_hdf5_attribute_read_int(root, dset_name, "OCTAVE_NEW_FORMAT", &val)) {
            return 0;
        }
        if (val == 1)
            return 1;
        else
            return 0;
    } else {
        return 0;
    }
    return 0;
}
