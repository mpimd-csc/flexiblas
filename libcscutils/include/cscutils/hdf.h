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


#ifndef CSC_HDF_H


#define CSC_HDF_H

#include "cscutils/cscutils_config.h"

#ifdef __cplusplus
extern "C" {
#endif

    #if defined(CSC_HAVE_ATTR_DEPRECATED)
     #define DEPRECATE __attribute__((deprecated))
     #define DEPRECATE_MSG(X) __attribute__((deprecated(X)))
    #else
     #warning "Your compiler does not support the deprecated attribute."
     #define DEPRECATE
     #define DEPRECATE_MSG(X)
    #endif

    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include <hdf5.h>
    #include <hdf5_hl.h>

    #ifdef __cplusplus
        #include <complex>
        #define dcomplex std::complex<double>
        #define fcomplex std::complex<float>
    #else
    #include <complex.h>
        #define dcomplex double complex
        #define fcomplex float complex

    #endif

    #include "cscutils/error_message.h"

    /**
     @file libcscutils/include/cscutils/hdf.h
      @defgroup hdf5 HDF5: Support function for HDF5 files.

       This part of the library contains routines to handle matrices, vectors and other high-level mathematical
       constructs easily in HDF5 files. In order to avoid the name clash with the HDF5 library the include file is
       name \b hdf.h .

       \attention This part of the library depends on the \ref error_message module.
       \attention The HDF5 library and our wrappers are not thread-safe.

      @addtogroup hdf5
      @{
    */

    #define CSC_HDF5_REAL_STR    "REAL"
    #define CSC_HDF5_COMPLEX_STR "COMPLEX"
    #define CSC_HDF5_VECTOR_STR  "VECTOR"
    #define CSC_HDF5_MATRIX_STR  "MATRIX"
    #define CSC_HDF5_SPCSR_STR   "SPCSR"
    #define CSC_HDF5_SPCSC_STR   "SPCSC"
    #define CSC_HDF5_SPCOO_STR   "SPCOO"

    typedef enum {
        CSC_HDF5_VECTOR = 0,
        CSC_HDF5_MATRIX = 1,
        CSC_HDF5_SPARSE_COORDINATE = 2,
        CSC_HDF5_SPARSE_CSC = 3,
        CSC_HDF5_SPARSE_CSR = 4,
        CSC_HDF5_SPARSE_MATLAB = 5,
        CSC_HDF5_UNKOWN_CONTENT = 999
    } csc_hdf5_content_t;

    herr_t csc_hdf5_content_type(hid_t root, hid_t *cnt, hid_t *cnt_file);

    int csc_hdf5_set_content(hid_t root, const char *dset_name, csc_hdf5_content_t content);
    int csc_hdf5_get_content(hid_t root, const char *dset_name, csc_hdf5_content_t *content);

    typedef enum {
        CSC_HDF5_REAL = 0,
        CSC_HDF5_DOUBLE = 0,
        CSC_HDF5_COMPLEX =1,
        CSC_HDF5_DOUBLE_COMPLEX = 1,
        CSC_HDF5_INTEGER8  = 2,
        CSC_HDF5_INTEGER16 = 3,
        CSC_HDF5_INTEGER32 = 4,
        CSC_HDF5_INTEGER64 = 5,
        CSC_HDF5_REAL_SINGLE = 6,
        CSC_HDF5_FLOAT = 6,
        CSC_HDF5_COMPLEX_SINGLE = 7,
        CSC_HDF5_FLOAT_COMPLEX = 7,
        CSC_HDF5_UNKOWN_FIELD = 999
    } csc_hdf5_field_t;

    /**
     * @brief Enumeration to support the compression of HDF5 datasets.
     *
     * The csc_hdf5_compression_t enumeration defines all compression algorithms supported
     * by our HDF5 wrapper. The availability depends on the compile time configuration of
     * libcscutils. To check whether a compression is available or not use \ref csc_hdf5_has_compression.
     *
     * @see csc_hdf5_has_compression
     *
     */
    typedef enum {
        CSC_HDF5_COMPRESS_NONE    = 0,      /*!< Disable compression. */
        CSC_HDF5_COMPRESS_DEFLATE = 1,      /*!< Use zlib/deflate compression. */
        CSC_HDF5_COMPRESS_BZIP2   = 2,      /*!< Use bzip2 compression. */
        CSC_HDF5_COMPRESS_XZ      = 3,      /*!< Use xz compression. */
        CSC_HDF5_COMPRESS_ZSTD    = 4       /*!< Use zstd compression. */
    } csc_hdf5_compression_t ;

    /**
     * @brief Check if a compression algorithm is available at runtime.
     * @param[in] comp      Compression algorithm to check
     * @return A non-zero value if the compression is available, zero otherwise
     *
     * The csc_hdf5_has_compression function checks whether a compression algorithm
     * is available at runtime or not.
     *
     * @see csc_hdf5_compression_t
     *
     */
    int csc_hdf5_has_compression(csc_hdf5_compression_t comp);

    typedef struct _csc_hdf5_options_t {
        csc_hdf5_compression_t compress;
        int deflate_level;
        size_t chunksize;
        int    complex_as_compound;
        int    matlab_attributes;
        int    octave_style;
    } csc_hdf5_options_t;

    #define CSC_HDF5_OPTIONS_INIT {CSC_HDF5_COMPRESS_NONE, 1, 0, 1, 1, 0}

    herr_t csc_hdf5_field_type(hid_t root, hid_t *ft, hid_t *ft_file);

    int csc_hdf5_set_field(hid_t root, const char *dset_name, csc_hdf5_field_t field);
    int csc_hdf5_get_field(hid_t root, const char *dset_name, csc_hdf5_field_t *field);

    int csc_hdf5_matrix_get_datatype(hid_t root, const char *dset_name, csc_hdf5_field_t *field);
    int csc_hdf5_sparse_get_datatype(hid_t root, const char *dset_name, csc_hdf5_field_t *field);


    void * csc_hdf5_matlab_header(const char * str);

    int csc_hdf5_register_filters();



    /**
     * @brief Open an HDF5 file.
     * @param[in] filename      Filename
     * @param[in] mode          Access mode
     * @return A negative value on failure or a valid, positive, HDF5 identifier
     *
     * The function csc_hdf5_open opens an HDF5 file. The access mode can be either
     * \b r, \b w, or \b rw whether the file is opened read-only, write-only or
     * for read-write access.
     *
     */
    hid_t csc_hdf5_open(const char *filename, const char *mode);

    /**
     * @brief Open an HDF5 file with an additional header.
     * @param[in] filename      Filename
     * @param[in] mode          Access mode
     * @param[in] header        Pointer to 512 byte memory.
     * @return A negative value on failure or a valid, positive, HDF5 identifier
     *
     * The function csc_hdf5_open2 opens an HDF5 file and adds an 512 byte long header
     * at the beginning of the file. The header can contain arbitray user supplied information.
     * In this way MATLAB compatible HDF5 files can be generated.. The access mode can be either
     * \b r, \b w, or \b rw whether the file is opened read-only, write-only or
     * for read-write access.
     *
     * @see csc_hdf5_open
     * @see csc_hdf5_open_matlab
     * @see csc_hdf5_matlab_header
     */
    hid_t csc_hdf5_open2(const char *filename, const char *mode, void *header);

    /**
     * @brief Open an HDF5 file with MATLAB compatible header.
     * @param[in] filename      Filename
     * @param[in] mode          Access mode
     * @return A negative value on failure or a valid, positive, HDF5 identifier
     *
     * The function csc_hdf5_open_matlab opens an HDF5 file with a MATLAB compatible header.
     * The access mode can be either  \b r, \b w, or \b rw whether the file is opened
     * read-only, write-only or for read-write access.
     *
     * @see csc_hdf5_matlab_header
     * @see csc_hdf5_open2
     * @see csc_hdf5_open
     *
     */
    hid_t csc_hdf5_open_matlab(const char *filename, const char *mode);


    /**
     * @brief Close an HDF5 file.
     * @p
     */
    hid_t csc_hdf5_close(hid_t root);

    hid_t csc_hdf5_group_open(hid_t root, const char *dset_name);
    hid_t csc_hdf5_group_close(hid_t group);
    int csc_hdf5_group_path_create(hid_t root, const char * path, int last);

    int csc_hdf5_group_exist(hid_t root, const char *dset_name);
    int csc_hdf5_dataset_exist(hid_t root, const char *dset_name);

    /**
     * @brief Check if a dataset has an attribute.
     * @param[in]   root        root id of the dataset
     * @param[in]   dset_name       name of the dataset
     * @param[in]   attr_name   name of the attribute
     * @return True ( > 0 ) if the attribute exists or False( = 0 ) otherwise.
     *
     * The csc_hdf5_attribute_exist function checks if the given dataset has an attribute
     * name "attr_name" and returns True ( > 1) if it exists.
     */
    int csc_hdf5_attribute_exist(hid_t root, const char *dset_name, const char *attr_name);

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
    #define csc_hdf5_attribute_write(root,dset_name, aname, aval) _Generic((aval), \
            char: csc_hdf5_attribute_write_char,\
            unsigned char: csc_hdf5_attribute_write_uchar,\
            int: csc_hdf5_attribute_write_int,\
            unsigned int: csc_hdf5_attribute_write_uint, \
            long: csc_hdf5_attribute_write_long, \
            unsigned long: csc_hdf5_attribute_write_ulong, \
            float: csc_hdf5_attribute_write_float, \
            double: csc_hdf5_attribute_write_double)(root,dset_name,aname,aval)
    #define csc_hdf5_attribute_read(root,dset_name, aname, aval) _Generic((aval), \
            char*: csc_hdf5_attribute_read_char, \
            unsigned char*: csc_hdf5_attribute_read_uchar,\
            int*: csc_hdf5_attribute_read_int,\
            unsigned int*: csc_hdf5_attribute_read_uint, \
            long*: csc_hdf5_attribute_read_long, \
            unsigned long*: csc_hdf5_attribute_read_ulong, \
            float*: csc_hdf5_attribute_read_float, \
            double*: csc_hdf5_attribute_read_double)(root,dset_name,aname,aval)
#endif

    int csc_hdf5_attribute_write_char(hid_t root, const char * dset_name, const char * aname, char aval);
    int csc_hdf5_attribute_write_uchar(hid_t root, const char * dset_name, const char * aname, unsigned char aval);
    int csc_hdf5_attribute_write_int(hid_t root, const char * dset_name, const char * aname, int aval);
    int csc_hdf5_attribute_write_uint(hid_t root, const char * dset_name, const char * aname, unsigned int aval);
    int csc_hdf5_attribute_write_long(hid_t root, const char * dset_name, const char * aname,long aval);
    int csc_hdf5_attribute_write_ulong(hid_t root, const char * dset_name, const char * aname, unsigned long aval);
    int csc_hdf5_attribute_write_float(hid_t root, const char * dset_name,  const char * aname, float  retval);
    int csc_hdf5_attribute_write_double(hid_t root, const char * dset_name,  const char * aname, double retval);
    int csc_hdf5_attribute_write_string(hid_t root, const char* dset_name, const char * aname, char * str);


    int csc_hdf5_attribute_read_char(hid_t root, const char * dset_name,  const char * aname, char * retval);
    int csc_hdf5_attribute_read_uchar(hid_t root, const char * dset_name,  const char * aname, unsigned char * retval);
    int csc_hdf5_attribute_read_int(hid_t root, const char * dset_name,  const char * aname, int * retval);
    int csc_hdf5_attribute_read_long(hid_t root, const char * dset_name,  const char * aname, long * retval);
    int csc_hdf5_attribute_read_uint(hid_t root, const char * dset_name,  const char * aname, unsigned int * retval);
    int csc_hdf5_attribute_read_ulong(hid_t root, const char * dset_name,  const char * aname, unsigned long * retval);
    int csc_hdf5_attribute_read_float(hid_t root, const char * dset_name,  const char * aname, float * retval);
    int csc_hdf5_attribute_read_double(hid_t root, const char * dset_name,  const char * aname, double * retval);
    int csc_hdf5_attribute_read_string(hid_t root, const char* dset_name, const char * aname, char ** str, size_t *len);

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
    #define csc_hdf5_read_matrix(root,dset_name,rows,cols,matrix,lda, opts) _Generic((matrix), \
                double _Complex *: csc_hdf5_read_double_complex_matrix,  \
                float _Complex *:  csc_hdf5_read_float_complex_matrix,\
                double *: csc_hdf5_read_double_matrix, \
                float *:  csc_hdf5_read_float_matrix, \
                char *: csc_hdf5_read_char_matrix, \
                short *: csc_hdf5_read_short_matrix, \
                int *: csc_hdf5_read_int_matrix, \
                long *: csc_hdf5_read_long_matrix)(root, dset_name, rows, cols, matrix, lda, opts)
    #define csc_hdf5_read_vector(root,dset_name,rows,matrix,opts) _Generic((matrix), \
                double _Complex *: csc_hdf5_read_double_complex_vector,  \
                float _Complex *:  csc_hdf5_read_float_complex_vector,\
                double *: csc_hdf5_read_double_vector, \
                float *:  csc_hdf5_read_float_vector, \
                char *: csc_hdf5_read_char_vector, \
                short *: csc_hdf5_read_short_vector, \
                int *: csc_hdf5_read_int_vector, \
                long *: csc_hdf5_read_long_vector)(root, dset_name, rows, matrix, opts)

#endif

    int csc_hdf5_read_double_matrix( hid_t root, const char* dset_name, size_t rows, size_t cols, double * matrix, size_t lda , csc_hdf5_options_t * opts);
    int csc_hdf5_read_float_matrix( hid_t root, const char* dset_name, size_t rows, size_t cols, float * matrix, size_t lda , csc_hdf5_options_t * opts);
    int csc_hdf5_read_double_complex_matrix( hid_t root, const char* dset_name, size_t rows, size_t cols, double complex * matrix, size_t lda , csc_hdf5_options_t * opts);
    int csc_hdf5_read_float_complex_matrix( hid_t root, const char* dset_name, size_t rows, size_t cols, float complex * matrix, size_t lda , csc_hdf5_options_t * opts);
    int csc_hdf5_read_char_matrix( hid_t root, const char* dset_name, size_t rows, size_t cols, char * matrix, size_t lda , csc_hdf5_options_t * opts);
    int csc_hdf5_read_short_matrix( hid_t root, const char* dset_name, size_t rows, size_t cols, short * matrix, size_t lda , csc_hdf5_options_t * opts);
    int csc_hdf5_read_int_matrix( hid_t root, const char* dset_name, size_t rows, size_t cols, int * matrix, size_t lda , csc_hdf5_options_t * opts);
    int csc_hdf5_read_long_matrix( hid_t root, const char* dset_name, size_t rows, size_t cols, long * matrix, size_t lda , csc_hdf5_options_t * opts);


    int csc_hdf5_read_double_vector( hid_t root, const char* dset_name, size_t rows, double * matrix, csc_hdf5_options_t * opts);
    int csc_hdf5_read_float_vector( hid_t root, const char* dset_name, size_t rows, float * matrix, csc_hdf5_options_t * opts);
    int csc_hdf5_read_double_complex_vector( hid_t root, const char* dset_name, size_t rows, double complex * matrix, csc_hdf5_options_t * opts);
    int csc_hdf5_read_float_complex_vector( hid_t root, const char* dset_name, size_t rows, float complex * matrix, csc_hdf5_options_t * opts);
    int csc_hdf5_read_char_vector( hid_t root, const char* dset_name, size_t rows, char * matrix, csc_hdf5_options_t * opts);
    int csc_hdf5_read_short_vector( hid_t root, const char* dset_name, size_t rows, short * matrix, csc_hdf5_options_t * opts);
    int csc_hdf5_read_int_vector( hid_t root, const char* dset_name, size_t rows, int * matrix, csc_hdf5_options_t * opts);
    int csc_hdf5_read_long_vector( hid_t root, const char* dset_name, size_t rows, long * matrix, csc_hdf5_options_t * opts);


    int csc_hdf5_read_string(hid_t root, const char * dset_name, char **str, size_t *len);
    int csc_hdf5_write_string(hid_t root, const char * dset_name, const char *str);

    int csc_hdf5_write_scalar(hid_t root, const char * dset_name, hid_t dtype, void *val);
    int csc_hdf5_write_int(hid_t root, const char * dset_name, int val);
    int csc_hdf5_write_uint(hid_t root, const char * dset_name, unsigned int val);
    int csc_hdf5_write_long(hid_t root, const char * dset_name, long val);
    int csc_hdf5_write_ulong(hid_t root, const char * dset_name, unsigned long val);
    int csc_hdf5_write_float(hid_t root, const char * dset_name, float val);
    int csc_hdf5_write_double(hid_t root, const char * dset_name, double val);

    int csc_hdf5_read_scalar(hid_t root, const char * dset_name, hid_t dtype, void *val);
    int csc_hdf5_read_int(hid_t root, const char * dset_name, int *val);
    int csc_hdf5_read_uint(hid_t root, const char * dset_name, unsigned int *val);
    int csc_hdf5_read_long(hid_t root, const char * dset_name, long *val);
    int csc_hdf5_read_ulong(hid_t root, const char * dset_name, unsigned long *val);
    int csc_hdf5_read_float(hid_t root, const char * dset_name, float *val);
    int csc_hdf5_read_double(hid_t root, const char * dset_name, double *val);









    int csc_hdf5_is_sparse(hid_t root, const char *dset_name);
    int csc_hdf5_is_octave(hid_t root, const char *dset_name);


    /**
     * @brief Create a HDF5 compound type to represent a complex number
     * @return The data type id or a negative value in case of an error.
     *
     * The csc_hdf5_complex_type function creates a HDF5 compound representing a double
     * precision complex number consisting of real and imaginary part compatible to the
     * one defined in ANSI C99.
     */
    hid_t csc_hdf5_complex_type(hid_t root);


    /**
     * @brief Check if the data set represents a vector.
     * @param[in]   root        root id of the dataset
     * @param[in]   dset_name   name of the data set
     * @return True (non zero) if the data set is a vector or False (zero) otherwise.
     *
     * The csc_hdf5_is_vector function checks the object_type attribute of the data set
     * and returns true if the object type represents a vector. If not or in case of an error
     * it returns false.
     */
    int csc_hdf5_is_vector(hid_t root, const char *dset_name) ;

    /**
     * @brief Check if the data set represents a matrix.
     * @param[in]   root        root id of the dataset
     * @param[in]   dset_name   name of the data set
     * @return True (non zero) if the data set is a matrix or False (zero) otherwise.
     *
     * The csc_hdf5_is_matrix function checks the object_type attribute of the data set
     * and returns true if the object type represents a matrix. If not or in case of an error
     * it returns false.
     */
    int csc_hdf5_is_matrix(hid_t root, const char *dset_name) ;

    /**
     * @brief Check if the data set represents a sparse csr matrix.
     * @param[in]   root        root id of the dataset
     * @param[in]   dset_name   name of the data set
     * @return True (non zero) if the data set is a sparse csr matrix or False (zero) otherwise.
     *
     * The csc_hdf5_is_spcsr function checks the object_type attribute of the data set
     * and returns true if the object type represents a sparse csr matrix. If not or in case of an error
     * it returns false.
     */
    int csc_hdf5_is_spcsr(hid_t root, const char *dset_name) ;

    /**
     * @brief Check if the data set represents a sparse csc matrix.
     * @param[in]   root        root id of the dataset
     * @param[in]   dset_name   name of the data set
     * @return True (non zero) if the data set is a sparse csc matrix or False (zero) otherwise.
     *
     * The csc_hdf5_is_spcsc function checks the object_type attribute of the data set
     * and returns true if the object type represents a sparse csc matrix. If not or in case of an error
     * it returns false.
     */
    int csc_hdf5_is_spcsc(hid_t root, const char *dset_name) ;

    /**
     * @brief Check if the data set represents a sparse coordinate matrix.
     * @param[in]   root        root id of the dataset
     * @param[in]   dset_name   name of the data set
     * @return True (non zero) if the data set is a sparse coordinate matrix or False (zero) otherwise.
     *
     * The csc_hdf5_is_spcoo function checks the object_type attribute of the data set
     * and returns true if the object type represents a sparse coordinate matrix. If not or in case of an error
     * it returns false.
     */
    int csc_hdf5_is_spcoo(hid_t root, const char *dset_name) ;

    /**
     * @brief Check if the data set contains real values
     * @param[in]   root        root id of the dataset
     * @param[in]   dset_name   name of the data set
     * @return True (non zero) if the data set contains real values or False (zero) otherwise.
     *
     * The csc_hdf5_is_real function checks the data_type attribute of the data set
     * and returns true if the data type represents real valued data. If not or in case of an error
     * it returns false.
     **/
    int csc_hdf5_is_real(hid_t root, const char *dset_name) ;

    /**
     * @brief Check if the data set contains real single precision values
     * @param[in]   root        root id of the dataset
     * @param[in]   dset_name   name of the data set
     * @return True (non zero) if the data set contains real values or False (zero) otherwise.
     *
     * The csc_hdf5_is_real function checks the data_type attribute of the data set
     * and returns true if the data type represents real valued data. If not or in case of an error
     * it returns false.
     **/
    int csc_hdf5_is_real_single(hid_t root, const char *dset_name) ;

    /**
     * @brief Check if the data set contains complex values
     * @param[in]   root        root id of the dataset
     * @param[in]   dset_name   name of the data set
     * @return True (non zero) if the data set contains complex values or False (zero) otherwise.
     *
     * The csc_hdf5_is_complex function checks the data_type attribute of the data set
     * and returns true if the data type represents complex valued data. If not or in case of an error
     * it returns false.
     **/
    int csc_hdf5_is_complex(hid_t root, const char *dset_name) ;



    /**
     * @brief Return a human readable representation of the datatype's class.
     * @param[in]  d    type id of the datatype.
     * @return a string containing the human readable representation of the datatype's class.
     *
     * The csc_hdf5_get_classname function returns a human readable representation of the H5Tget_class
     * function.
     */
    char *csc_hdf5_get_classname(hid_t d);


    /**
     * @brief Print the native of a given data type.
     * @param[in] dtype_id ID of the data type.
     * @param[in] direction Search direction for the native type.
     *
     * The csc_hdf5_print_native_type function search for the first fitting
     * native data type using H5Tget_native_type. Therefore a search direction
     * can be specified. This is either H5T_DIR_ASCEND or H5T_DIR_DESCEND.
     */
    void csc_hdf5_print_native_type(hid_t dtype_id, H5T_direction_t direction );




    /**
     * @brief Enable or disable compression of datasets
     * @param[in] comp  Compression Type.
     *
     * The csc_hdf5_set_compression function enables or disables the compression of
     * the data sets.
     */
    void csc_hdf5_set_compression(csc_hdf5_compression_t comp);
    csc_hdf5_compression_t csc_hdf5_get_compression();
    int  csc_hdf5_get_compression_int();


    int csc_hdf5_exist(hid_t h5file, const char *name);



    /**
     * @brief Wrapper around H5LTmake_dataset with compression support.
     * @param[in]   loc         root location of the new dataset
     * @param[in]   dset_name   name of the new dataset
     * @param[in]   rank        number of dimension of the dataset
     * @param[in]   dims        array containing the dimensions.
     * @param[in]   tid         id of the data type
     * @param[in]   data        pointer to the data
     * @return zero on success or a negative error code
     *
     * The csc_hdf5_create_dataset function is a wrapper around H5LTmake_dataset with the same calling
     * sequence. The only difference is that is enables the compression of the dataset depending on the value
     * set by \ref csc_hdf5_set_compression before. For a detailed description please read the HDF5 documentation.
     *
     */
    int csc_hdf5_create_dataset(hid_t loc, const char *dset_name, int rank, const hsize_t *dims, hid_t tid, const void *data);

    /**
     * @brief Return the length of a vector in an HDF5 storage.
     * @param[in]   root        root id of the dataset
     * @param[in]   dset_name   name of the dataset.
     * @return the length of the vector or a negative error code.
     *
     * The csc_hdf5_vector_len function returns the length of a vector stored
     * in a HDF5 file and addressed by root and dset_name.
     */
    long csc_hdf5_vector_len(hid_t root, const char *dset_name);

    csc_hdf5_field_t csc_hdf5_vector_field(hid_t root, const char *dset_name);


    /**
     * @brief Read a vector from an HDF5 dataset.
     * @param[in]  root         root id of the dataset
     * @param[in]  field        Number field of the vector
     * @param[in]  dset_name    name of the dataset
     * @param[out] vector       preallocated vector
     * @return zero on success or a non zero error code otherwise
     *
     * The csc_hdf5_vector_read function read a vector from the specified dataset. The output
     * vector needs to be a preallocated array of the correct type and dimension. The dimension
     * of the vector can be retrieved using \ref csc_hdf5_vector_len and the data type is either
     * double or double complex and can be checked using \ref csc_hdf5_is_real or \ref csc_hdf5_is_complex.
     */
    int csc_hdf5_vector_read(csc_hdf5_field_t field, hid_t root, const char *dset_name, void * vector) DEPRECATE_MSG("Use csc_hdf5_read_vector instead.");

    /**
     * @brief Write a real(double) vector to a HDF5 dataset.
     * @param[in] root          root id of the dataset
     * @param[in] field         Number field of the values to write.
     * @param[in] dset_name     name of the dataset
     * @param[in] len           length of the vector
     * @param[in] vector        pointer to the vector
     * @return zero on success or a non zero error code otherwise
     *
     * The csc_hdf5_vector_write function writes a vector of length len into
     * the specified dataset. The field parameter gives the type of the vector.
     * If the compression is set using \ref csc_hdf5_set_compression
     * the data will be compressed during the write operation.
     */
    int csc_hdf5_vector_write(csc_hdf5_field_t field, hid_t root, const char *dset_name, size_t len, void *vector);


    /**
     * @brief Write a real(double) matrix to a HDF5 dataset.
     * @param[in] root      root id of the dataset
     * @param[in] dset_name     name of the dataset
     * @param[in] rows      number of rows of the matrix
     * @param[in] cols      number of columns of the matrix
     * @param[in] ld        leading (row) dimension of the matrix
     * @param[in] matrix        pointer to the matrix
     * @return zero on success or a non zero error code otherwise
     *
     * The csc_hdf5_matrix_write_real function writes a double precision matrix into
     * the specified dataset. The matrix must be given in Fortran storage scheme and
     * it is stored as transpose of it self in the dataset. The reason behind that is
     * that normally all numerical algorithms require the Fortran storage but HDF5 stores
     * multidimensional arrays in C style.
     * If the compression is set using \ref csc_hdf5_set_compression
     * the data will be compressed during the write operation.
     */
    int csc_hdf5_matrix_write_real(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t ld, double *matrix);
    int csc_hdf5_matrix_write_real_single(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t ld, float *matrix);

    /**
     * @brief Write a complex(double) matrix to a HDF5 dataset.
     * @param[in] root      root id of the dataset
     * @param[in] dset_name     name of the dataset
     * @param[in] rows      number of rows of the matrix
     * @param[in] cols      number of columns of the matrix
     * @param[in] ld        leading (row) dimension of the matrix
     * @param[in] matrix        pointer to the matrix
     * @return zero on success or a non zero error code otherwise
     *
     * The csc_hdf5_matrix_write_complex function writes a double precision matrix into
     * the specified dataset. The matrix must be given in Fortran storage scheme and
     * it is stored as transpose of it self in the dataset. The reason behind that is
     * that normally all numerical algorithms require the Fortran storage but HDF5 stores
     * multidimensional arrays in C style.
     * If the compression is set using \ref csc_hdf5_set_compression
     * the data will be compressed during the write operation.
     */
    int csc_hdf5_matrix_write_complex(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t ld, dcomplex *matrix);


    int csc_hdf5_matrix_read_real(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t ld, double *matrix);
    int csc_hdf5_matrix_read_real_single(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t ld, float *matrix);

    int csc_hdf5_matrix_read_complex(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t ld, dcomplex *matrix);

    int csc_hdf5_sparse_write (csc_hdf5_content_t type, csc_hdf5_field_t number_field, csc_hdf5_field_t integer_field, hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz, void *rowptr, void *colptr, void *values);
    int csc_hdf5_sparse_read (csc_hdf5_content_t *type, csc_hdf5_field_t *number_field, csc_hdf5_field_t integer_field, hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz, void *rowptr, void *colptr, void *values);


#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
    #define csc_hdf5_write_matrix(root,dset_name,rows,cols,matrix,lda, opts) _Generic((matrix), \
                double _Complex *: csc_hdf5_write_double_complex_matrix,  \
                float _Complex *:  csc_hdf5_write_float_complex_matrix,\
                double *: csc_hdf5_write_double_matrix, \
                float *:  csc_hdf5_write_float_matrix)(root, dset_name, rows, cols, matrix, lda, opts)
#endif

    int csc_hdf5_write_double_matrix( hid_t root, const char* dset_name, size_t rows, size_t cols, double * matrix, size_t lda, csc_hdf5_options_t * opts);
    int csc_hdf5_write_float_matrix( hid_t root, const char* dset_name, size_t rows, size_t cols, float * matrix, size_t lda , csc_hdf5_options_t * opts);
    int csc_hdf5_write_double_complex_matrix( hid_t root, const char* dset_name, size_t rows, size_t cols, dcomplex * matrix, size_t lda , csc_hdf5_options_t * opts);
    int csc_hdf5_write_float_complex_matrix( hid_t root, const char* dset_name, size_t rows, size_t cols, fcomplex * matrix, size_t lda , csc_hdf5_options_t * opts);


#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
    #define csc_hdf5_write_vector(root,dset_name,rows, matrix, opts) _Generic((matrix), \
                double _Complex *: csc_hdf5_write_double_complex_vector,  \
                float _Complex *:  csc_hdf5_write_float_complex_vector,\
                double *: csc_hdf5_write_double_vector, \
                float *:  csc_hdf5_write_float_vector)(root, dset_name, rows, matrix, opts)
#endif

    int csc_hdf5_write_double_vector( hid_t root, const char* dset_name, size_t rows, double * matrix, csc_hdf5_options_t * opts);
    int csc_hdf5_write_float_vector( hid_t root, const char* dset_name, size_t rows, float * matrix, csc_hdf5_options_t * opts);
    int csc_hdf5_write_double_complex_vector( hid_t root, const char* dset_name, size_t rows, double complex * matrix, csc_hdf5_options_t * opts);
    int csc_hdf5_write_float_complex_vector( hid_t root, const char* dset_name, size_t rows, float complex * matrix, csc_hdf5_options_t * opts);

    int csc_hdf5_write_double_sparse_matrix_int(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz, int * rowptr, int *colptr, double *data, csc_hdf5_options_t *opts);
    int csc_hdf5_write_double_sparse_matrix_uint(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz, unsigned int * rowptr, unsigned int *colptr, double *data, csc_hdf5_options_t *opts);
    int csc_hdf5_write_double_sparse_matrix_long(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz, long * rowptr, long *colptr, double *data, csc_hdf5_options_t *opts);
    int csc_hdf5_write_double_sparse_matrix_ulong(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz, unsigned long * rowptr, unsigned long *colptr, double *data, csc_hdf5_options_t *opts);

    int csc_hdf5_write_float_sparse_matrix_int(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz, int * rowptr, int *colptr, float *data, csc_hdf5_options_t *opts);
    int csc_hdf5_write_float_sparse_matrix_uint(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz, unsigned int * rowptr, unsigned int *colptr, float *data, csc_hdf5_options_t *opts);
    int csc_hdf5_write_float_sparse_matrix_long(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz, long * rowptr, long *colptr, float *data, csc_hdf5_options_t *opts);
    int csc_hdf5_write_float_sparse_matrix_ulong(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz, unsigned long * rowptr, unsigned long *colptr, float *data, csc_hdf5_options_t *opts);

    int csc_hdf5_write_double_complex_sparse_matrix_int(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz, int * rowptr, int *colptr, double complex *data, csc_hdf5_options_t *opts);
    int csc_hdf5_write_double_complex_sparse_matrix_uint(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz, unsigned int * rowptr, unsigned int *colptr, double complex *data, csc_hdf5_options_t *opts);
    int csc_hdf5_write_double_complex_sparse_matrix_long(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz, long * rowptr, long *colptr, double complex *data, csc_hdf5_options_t *opts);
    int csc_hdf5_write_double_complex_sparse_matrix_ulong(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz, unsigned long * rowptr, unsigned long *colptr, double complex *data, csc_hdf5_options_t *opts);

    int csc_hdf5_write_float_complex_sparse_matrix_int(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz, int * rowptr, int *colptr, float complex *data, csc_hdf5_options_t *opts);
    int csc_hdf5_write_float_complex_sparse_matrix_uint(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz, unsigned int * rowptr, unsigned int *colptr, float complex *data, csc_hdf5_options_t *opts);
    int csc_hdf5_write_float_complex_sparse_matrix_long(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz, long * rowptr, long *colptr, float complex *data, csc_hdf5_options_t *opts);
    int csc_hdf5_write_float_complex_sparse_matrix_ulong(hid_t root, const char *dset_name, size_t rows, size_t cols, size_t nnz, unsigned long * rowptr, unsigned long *colptr, float complex *data, csc_hdf5_options_t *opts);


    int csc_hdf5_read_double_sparse_matrix_int(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        int *rowptr, int *colptr, double * values, csc_hdf5_options_t *opts);
    int csc_hdf5_read_double_sparse_matrix_uint(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        unsigned int *rowptr, unsigned int *colptr, double * values, csc_hdf5_options_t *opts);
    int csc_hdf5_read_double_sparse_matrix_long(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        long *rowptr, long *colptr, double * values, csc_hdf5_options_t *opts);
    int csc_hdf5_read_double_sparse_matrix_ulong(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        unsigned long *rowptr, unsigned long *colptr, double * values, csc_hdf5_options_t *opts);

    int csc_hdf5_read_float_sparse_matrix_int(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        int *rowptr, int *colptr, float * values, csc_hdf5_options_t *opts);
    int csc_hdf5_read_float_sparse_matrix_uint(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        unsigned int *rowptr, unsigned int *colptr, float * values, csc_hdf5_options_t *opts);
    int csc_hdf5_read_float_sparse_matrix_long(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        long *rowptr, long *colptr, float * values, csc_hdf5_options_t *opts);
    int csc_hdf5_read_float_sparse_matrix_ulong(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        unsigned long *rowptr, unsigned long *colptr, float * values, csc_hdf5_options_t *opts);

    int csc_hdf5_read_double_complex_sparse_matrix_int(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        int *rowptr, int *colptr, double complex * values, csc_hdf5_options_t *opts);
    int csc_hdf5_read_double_complex_sparse_matrix_uint(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        unsigned int *rowptr, unsigned int *colptr, double complex * values, csc_hdf5_options_t *opts);
    int csc_hdf5_read_double_complex_sparse_matrix_long(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        long *rowptr, long *colptr, double complex * values, csc_hdf5_options_t *opts);
    int csc_hdf5_read_double_complex_sparse_matrix_ulong(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        unsigned long *rowptr, unsigned long *colptr, double complex * values, csc_hdf5_options_t *opts);

    int csc_hdf5_read_float_complex_sparse_matrix_int(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        int *rowptr, int *colptr, float complex * values, csc_hdf5_options_t *opts);
    int csc_hdf5_read_float_complex_sparse_matrix_uint(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        unsigned int *rowptr, unsigned int *colptr, float complex * values, csc_hdf5_options_t *opts);
    int csc_hdf5_read_float_complex_sparse_matrix_long(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        long *rowptr, long *colptr, float complex * values, csc_hdf5_options_t *opts);
    int csc_hdf5_read_float_complex_sparse_matrix_ulong(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz,
        unsigned long *rowptr, unsigned long *colptr, float complex * values, csc_hdf5_options_t *opts);





    /**
     * @brief Return the size of a matrix inside an HDF5 storage.
     * @param[in]   root        root id of the dataset
     * @param[in]   dset_name   name of the dataset.
     * @param[out]  rows        The number of rows of the matrix
     * @param[out]  cols        The number of cols of the matrix.
     * @return zero on success or a non-zero error code.
     *
     * The csc_hdf5_matrix_size function reads the size of a matrix from a
     * given data set addressed by root and dset_name.
     */

    int csc_hdf5_matrix_size(hid_t root, const char *dset_name, size_t *rows, size_t *cols);
    int csc_hdf5_sparse_size(hid_t root, const char *dset_name, size_t *rows, size_t *cols, size_t *nnz);



    /**
     * @}
     */

#undef DEPRECATE
#undef DEPRECATE_MSG
#ifdef __cplusplus
};
#endif

#endif /* end of include guard: CSC_HDF_H */
