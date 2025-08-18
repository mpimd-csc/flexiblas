/*
 * CSCUTILS - A collection of various software routines uses in CSC projects
 * Copyright (C) 2015-2020 Martin Koehler
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

#include <hdf5.h>
#include <hdf5_hl.h>

#include "cscutils/hdf.h"
#include "csc_hdf5_common.h"
#include "cscutils/strutils.h"

hid_t csc_hdf5_open(const char *filename, const char *mode)
{
    hid_t file = -1;
    H5E_auto2_t func;
    void *data;
    herr_t err;
    char *fni;

    csc_hdf5_register_filters();

    err = H5Eget_auto2( H5E_DEFAULT, &func, &data);
    if ( err < 0 ) {
        csc_error_message("Failed to get current error stack function.\n");
        return -1;
    }

    fni = strdup(filename);
    if (!fni) return -1;
    csc_strremovedup(fni, '/');

    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);
    if ( csc_strcasecmp(mode, "r") == 0  ) {
        file = H5Fopen(fni, H5F_ACC_RDONLY, H5P_DEFAULT);
        if ( file < 0 ) {
               csc_error_message("Failed to open %s.\n", fni);
        }
    } else if ( csc_strcasecmp(mode, "w") == 0
            || csc_strcasecmp(mode,"w+") == 0 ) {
        file = H5Fcreate(fni, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    } else if ( csc_strcasecmp(mode, "rw" ) == 0 ) {
        if (  H5Fis_hdf5(fni) > 0) {
            file = H5Fopen(fni, H5F_ACC_RDWR, H5P_DEFAULT);
            if ( file < 0 ) {
                csc_error_message("Failed to open %s.\n", fni);
            }
        } else {
            file = H5Fcreate(fni,  H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
            if ( file < 0 ) {
                csc_error_message("Failed to create %s and truncate.\n", fni);
            }
        }
    }
    H5Eset_auto2(H5E_DEFAULT, func, data);
    free(fni);
    return file;
}


hid_t csc_hdf5_open2(const char *filename, const char *mode, void *header)
{
    hid_t file = -1;
    H5E_auto2_t func;
    void *data;
    herr_t err;
    hid_t plist   = H5P_DEFAULT ;
    hid_t plist_ap = H5P_DEFAULT;
    int ret = 0;
    char *fni = NULL;

    if (header == NULL || csc_strcasecmp(mode, "r") == 0 ) {
        return csc_hdf5_open(filename, mode);
    }

    csc_hdf5_register_filters();

    err = H5Eget_auto2( H5E_DEFAULT, &func, &data);
    if ( err < 0 ) {
        csc_error_message("Failed to get current error stack function.\n");
        return -1;
    }

    fni = strdup(filename);
    if (!fni) return -1;
    csc_strremovedup(fni, '/');

    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);
    if ( csc_strcasecmp(mode, "w") == 0
            || csc_strcasecmp(mode,"w+") == 0 ) {
        if (header != NULL ) {
            plist =  H5Pcreate(H5P_FILE_CREATE);
            if ( plist < 0 ) {
                ret = -1;
                goto end;
            }
            if ( H5Pset_userblock(plist,512) < 0 ) {
                ret = -1;
                goto end;
            }
            plist_ap = H5Pcreate(H5P_FILE_ACCESS);
            if ( plist_ap < 0 ) {
                ret = -1;
                goto end;
            }

#if H5_VERSION_GE(1,10,2)
            if ( header != NULL) {
                /* Matlab uses at most version 1.8  */
                if ( H5Pset_libver_bounds(plist_ap,H5F_LIBVER_EARLIEST,H5F_LIBVER_V18) < 0 ) {
                    ret = -1;
                    goto end;
                }
            }
#endif

        }
        file = H5Fcreate(fni, H5F_ACC_TRUNC, plist, plist_ap);

        /* Write the header  */
        if (header) {
            FILE *fp;

            H5Fclose(file);

            fp = fopen(fni, "r+b");
            if ( !fp) {
                ret = -1;
                goto end;
            }
            (void) fseek(fp, 0, SEEK_SET);
            fwrite(header, 1, 512, fp);
            fclose(fp);

            file = H5Fopen(fni,H5F_ACC_RDWR,plist_ap);
        }


    } else if ( csc_strcasecmp(mode, "rw" ) == 0 ) {
        if (  H5Fis_hdf5(fni) > 0) {
            file = H5Fopen(fni, H5F_ACC_RDWR, H5P_DEFAULT);
            if ( file < 0 ) {
                csc_error_message("Failed to open %s.\n", fni);
            }
        } else {
            return csc_hdf5_open2(fni, "w", header);
        }
    }

end:
    if ( plist_ap != H5P_DEFAULT) H5Pclose(plist_ap);
    if ( plist != H5P_DEFAULT ) H5Pclose(plist);
    if ( fni) free(fni);
    H5Eset_auto2(H5E_DEFAULT, func, data);
    if ( ret < 0 ) return -1;

    return file;
}


#pragma pack(1)
struct matlab_header_t
{
    char text[116];
    char subsys_offset[8];
    uint16_t version;
    uint16_t endian;
    char blank[384];
};

void * csc_hdf5_matlab_header(const char * str){
    char * ptr = calloc(sizeof(char), 512);
    struct matlab_header_t *hptr = (struct matlab_header_t*) (void *) ptr;
    time_t  t = time(NULL);

    memset(ptr, 0, 512);
    memset(hptr->text, ' ', 116);
    hptr->text[115] = '\0';
    if ( str ) {
        snprintf(hptr->text, 116, "%s", str);
    } else {
        snprintf(hptr->text, 116, "MATLAB 7.3 MAT-file, Created by libcscutils on %s HDF5 schema 0.5", ctime(&t));
    }
    memset(hptr->subsys_offset,' ',8);
    hptr->version = 0x0200;
    hptr->endian = 0x4d49;
    return (void*) ptr;
}

hid_t csc_hdf5_open_matlab(const char *filename, const char *mode)
{
    void *mlhdr = csc_hdf5_matlab_header(NULL);
    hid_t ret;
    ret = csc_hdf5_open2(filename, mode, mlhdr);
    free(mlhdr);
    return ret;
}

hid_t csc_hdf5_close(hid_t root)
{
    herr_t err = H5Fflush(root,H5F_SCOPE_GLOBAL);
    if ( err < 0 ) {
        csc_error_message("Failed to fflush file.\n");
    }
    err = H5Fclose(root);
    if ( err < 0 ) {
        csc_error_message("Failed to close file.\n");
    }
    return err;
}

