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


#ifndef HDF_COMMON_H

#define HDF_COMMON_H

#ifdef __cplusplus
extern "C" {
#endif
    extern int CSC_HDF5_COMPRESSION;

#define HDF5_GET_ATTR_ULONG(root,dset_name,attrname,dest) do { \
    unsigned long _tmp; herr_t _err;  \
    _err = csc_hdf5_attribute_read_ulong(root, dset_name, attrname, &_tmp); \
    if ( _err != 0 ) {\
        csc_error_message("Cannot get the attribute %s from the data set.\n", attrname); \
        return -1; \
    }\
    (dest) = _tmp;  \
    } while(0);

#ifdef __cplusplus
};
#endif

#endif /* end of include guard: HDF_COMMON_H */
