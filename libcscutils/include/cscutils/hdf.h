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
#include <stddef.h>

#ifdef __cplusplus
extern "C"
{
#endif

#if defined(CSC_HAVE_ATTR_DEPRECATED)
#define DEPRECATE __attribute__((deprecated))
#define DEPRECATE_MSG(X) __attribute__((deprecated(X)))
#else
#if __STDC_VERSION__ >= 202311L
#warning "Your compiler does not support the deprecated attribute."
#endif
#define DEPRECATE
#define DEPRECATE_MSG(X)
#endif

#include <hdf5.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

/** @name HDF5 Content Type Strings
 *  @{
 *  String constants used to identify the content type of datasets in HDF5 files.
 */
#define CSC_HDF5_REAL_STR "REAL"       /*!< String identifier for real data type. */
#define CSC_HDF5_COMPLEX_STR "COMPLEX" /*!< String identifier for complex data type. */
#define CSC_HDF5_VECTOR_STR "VECTOR"   /*!< String identifier for vector data. */
#define CSC_HDF5_MATRIX_STR "MATRIX"   /*!< String identifier for matrix data. */
#define CSC_HDF5_SPCSR_STR "SPCSR"     /*!< String identifier for sparse CSR matrix. */
#define CSC_HDF5_SPCSC_STR "SPCSC"     /*!< String identifier for sparse CSC matrix. */
#define CSC_HDF5_SPCOO_STR "SPCOO"     /*!< String identifier for sparse COO matrix. */
                                       /** @} */

    /**
     * @brief Enumeration for content types of HDF5 datasets.
     *
     * The csc_hdf5_content_t enumeration defines the various types of data structures
     * that can be stored in HDF5 files, including vectors, matrices, and sparse matrices
     * in different formats.
     */
    typedef enum
    {
        CSC_HDF5_VECTOR = 0,            /*!< Vector data structure. */
        CSC_HDF5_MATRIX = 1,            /*!< Matrix data structure. */
        CSC_HDF5_SPARSE_COORDINATE = 2, /*!< Sparse matrix in COO (coordinate) format. */
        CSC_HDF5_SPARSE_CSC = 3,        /*!< Sparse matrix in CSC (compressed sparse column) format. */
        CSC_HDF5_SPARSE_CSR = 4,        /*!< Sparse matrix in CSR (compressed sparse row) format. */
        CSC_HDF5_SPARSE_MATLAB = 5,     /*!< Sparse matrix in MATLAB format. */
        CSC_HDF5_UNKOWN_CONTENT = 999   /*!< Unknown or unsupported content type. */
    } csc_hdf5_content_t;

    /**
     * @brief Determine the content type of an HDF5 dataset.
     * @param[in] root          HDF5 group identifier
     * @param[out] cnt          Pointer to store the detected content type
     * @param[out] cnt_file     Pointer to store the file content type
     * @return herr_t           Error code (0 on success, negative on failure)
     *
     * The csc_hdf5_content_type function analyzes the specified dataset and determines
     * its content type by examining the associated attributes.
     */
    herr_t csc_hdf5_content_type(hid_t root, hid_t* cnt, hid_t* cnt_file);

    /**
     * @brief Set the content type attribute of an HDF5 dataset.
     * @param[in] root          HDF5 group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] content       Content type to set
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_set_content function creates and attaches an attribute to the
     * specified dataset to indicate its content type.
     */
    int csc_hdf5_set_content(hid_t root, const char* dset_name, csc_hdf5_content_t content);

    /**
     * @brief Get the content type attribute of an HDF5 dataset.
     * @param[in] root          HDF5 group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] content      Pointer to store the detected content type
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_get_content function reads the content type attribute from
     * the specified dataset.
     */
    int csc_hdf5_get_content(hid_t root, const char* dset_name, csc_hdf5_content_t* content);

    /**
     * @brief Enumeration for numeric field types in HDF5 datasets.
     *
     * The csc_hdf5_field_t enumeration defines the numeric data types that can
     * be stored in HDF5 files, including real, complex, and integer types of
     * various precisions.
     */
    typedef enum
    {
        CSC_HDF5_REAL = 0,           /*!< Real double precision (double). */
        CSC_HDF5_DOUBLE = 0,         /*!< Alias for CSC_HDF5_REAL. */
        CSC_HDF5_COMPLEX = 1,        /*!< Complex double precision (double complex). */
        CSC_HDF5_DOUBLE_COMPLEX = 1, /*!< Alias for CSC_HDF5_COMPLEX. */
        CSC_HDF5_INTEGER8 = 2,       /*!< 8-bit signed integer. */
        CSC_HDF5_INTEGER16 = 3,      /*!< 16-bit signed integer. */
        CSC_HDF5_INTEGER32 = 4,      /*!< 32-bit signed integer. */
        CSC_HDF5_INTEGER64 = 5,      /*!< 64-bit signed integer. */
        CSC_HDF5_REAL_SINGLE = 6,    /*!< Real single precision (float). */
        CSC_HDF5_FLOAT = 6,          /*!< Alias for CSC_HDF5_REAL_SINGLE. */
        CSC_HDF5_COMPLEX_SINGLE = 7, /*!< Complex single precision (float complex). */
        CSC_HDF5_FLOAT_COMPLEX = 7,  /*!< Alias for CSC_HDF5_COMPLEX_SINGLE. */
        CSC_HDF5_UNKOWN_FIELD = 999  /*!< Unknown or unsupported field type. */
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
    typedef enum
    {
        CSC_HDF5_COMPRESS_NONE = 0,    /*!< Disable compression. */
        CSC_HDF5_COMPRESS_DEFLATE = 1, /*!< Use zlib/deflate compression. */
        CSC_HDF5_COMPRESS_BZIP2 = 2,   /*!< Use bzip2 compression. */
        CSC_HDF5_COMPRESS_XZ = 3,      /*!< Use xz compression. */
        CSC_HDF5_COMPRESS_ZSTD = 4     /*!< Use zstd compression. */
    } csc_hdf5_compression_t;

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

    /**
     * @brief Structure to hold HDF5 I/O options.
     *
     * The csc_hdf5_options_t structure contains various options that control
     * how data is read from or written to HDF5 files, including compression
     * settings, chunking, and data representation options.
     */
    typedef struct _csc_hdf5_options_t
    {
        csc_hdf5_compression_t compress; /*!< Compression algorithm to use. */
        int deflate_level;               /*!< Compression level for deflate (0-9). */
        size_t chunksize;                /*!< Chunk size for chunked datasets. */
        int complex_as_compound;         /*!< Store complex as compound type. */
        int matlab_attributes;           /*!< Include MATLAB-specific attributes. */
        int octave_style;                /*!< Use Octave-compatible format. */
    } csc_hdf5_options_t;

    /**
     * @brief Default initialization values for csc_hdf5_options_t structure.
     *
     * The CSC_HDF5_OPTIONS_INIT macro provides default values for initializing
     * a csc_hdf5_options_t structure. The default settings disable compression,
     * disable chunking, store complex numbers as compound type, include MATLAB
     * attributes, and use standard (non-Octave) format.
     *
     * @see csc_hdf5_options_t
     */
#define CSC_HDF5_OPTIONS_INIT                                                                                                        \
    {                                                                                                                                \
        CSC_HDF5_COMPRESS_NONE, 1, 0, 1, 1, 0                                                                                        \
    }

    /**
     * @brief Determine the numeric field type of an HDF5 dataset.
     * @param[in] root          HDF5 group identifier
     * @param[out] ft           Pointer to store the detected field type
     * @param[out] ft_file      Pointer to store the file field type
     * @return herr_t           Error code (0 on success, negative on failure)
     *
     * The csc_hdf5_field_type function analyzes the specified dataset and determines
     * its numeric field type by examining the associated attributes.
     */
    herr_t csc_hdf5_field_type(hid_t root, hid_t* ft, hid_t* ft_file);

    /**
     * @brief Set the numeric field type attribute of an HDF5 dataset.
     * @param[in] root          HDF5 group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] field         Field type to set
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_set_field function creates and attaches an attribute to the
     * specified dataset to indicate its numeric field type.
     */
    int csc_hdf5_set_field(hid_t root, const char* dset_name, csc_hdf5_field_t field);

    /**
     * @brief Get the numeric field type attribute of an HDF5 dataset.
     * @param[in] root          HDF5 group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] field        Pointer to store the detected field type
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_get_field function reads the field type attribute from
     * the specified dataset.
     */
    int csc_hdf5_get_field(hid_t root, const char* dset_name, csc_hdf5_field_t* field);

    /**
     * @brief Get the numeric field type of a matrix dataset.
     * @param[in] root          HDF5 group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] field        Pointer to store the detected field type
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_matrix_get_datatype function extracts the field type from
     * a matrix dataset's attributes.
     */
    int csc_hdf5_matrix_get_datatype(hid_t root, const char* dset_name, csc_hdf5_field_t* field);

    /**
     * @brief Get the numeric field type of a sparse matrix dataset.
     * @param[in] root          HDF5 group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] field        Pointer to store the detected field type
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_sparse_get_datatype function extracts the field type from
     * a sparse matrix dataset's attributes.
     */
    int csc_hdf5_sparse_get_datatype(hid_t root, const char* dset_name, csc_hdf5_field_t* field);

    /**
     * @brief Create a MATLAB header string.
     * @param[in] str           String to encode in the header
     * @return void*            Pointer to a 512-byte buffer containing the header
     *
     * The csc_hdf5_matlab_header function creates a 512-byte header compatible
     * with MATLAB's HDF5 file format specification. The input string is encoded
     * into the header buffer.
     */
    void* csc_hdf5_matlab_header(const char* str);

    /**
     * @brief Register custom HDF5 filter plugins.
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_register_filters function registers all available custom
     * compression filters (bzip2, xz, zstd) with the HDF5 library. This function
     * is called automatically when compression is enabled.
     */
    int csc_hdf5_register_filters(void);

    /**
     * @brief Open an HDF5 file.
     * @param[in] filename      Filename
     * @param[in] mode          Access mode: "r" (read-only), "w" (write-only), or "rw" (read-write)
     * @return hid_t            A negative value on failure or a valid, positive, HDF5 identifier
     *
     * The function csc_hdf5_open opens an HDF5 file. The access mode can be either
     * \b r, \b w, or \b rw whether the file is opened read-only, write-only or
     * for read-write access.
     *
     */
    hid_t csc_hdf5_open(const char* filename, const char* mode);

    /**
     * @brief Open an HDF5 file with an additional header.
     * @param[in] filename      Filename
     * @param[in] mode          Access mode: "r" (read-only), "w" (write-only), or "rw" (read-write)
     * @param[in] header        Pointer to 512 byte memory containing custom header data.
     * @return hid_t            A negative value on failure or a valid, positive, HDF5 identifier
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
    hid_t csc_hdf5_open2(const char* filename, const char* mode, void* header);

    /**
     * @brief Open an HDF5 file with MATLAB compatible header.
     * @param[in] filename      Filename
     * @param[in] mode          Access mode: "r" (read-only), "w" (write-only), or "rw" (read-write)
     * @return hid_t            A negative value on failure or a valid, positive, HDF5 identifier
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
    hid_t csc_hdf5_open_matlab(const char* filename, const char* mode);

    /**
     * @brief Close an HDF5 file.
     * @param[in] root          HDF5 file identifier
     * @return hid_t            HDF5 identifier of the closed file
     *
     * The function csc_hdf5_close closes an HDF5 file and releases all associated resources.
     */
    hid_t csc_hdf5_close(hid_t root);

    /**
     * @brief Open or create a group within an HDF5 file.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the group to open or create
     * @return hid_t            HDF5 group identifier, negative on failure
     *
     * The csc_hdf5_group_open function opens an existing group or creates a new group
     * with the specified name within the given HDF5 location.
     */
    hid_t csc_hdf5_group_open(hid_t root, const char* dset_name);

    /**
     * @brief Close an HDF5 group.
     * @param[in] group         HDF5 group identifier
     * @return hid_t            HDF5 identifier of the closed group
     *
     * The csc_hdf5_group_close function closes the specified group and releases
     * all associated resources.
     */
    hid_t csc_hdf5_group_close(hid_t group);

    /**
     * @brief Create a path of nested groups.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] path          Path string with group names separated by "/"
     * @param[in] last          Flag indicating if this is the last group in the path
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_group_path_create function creates a sequence of nested groups
     * according to the specified path. Each group in the path is created if it
     * does not already exist.
     */
    int csc_hdf5_group_path_create(hid_t root, const char* path, int last);

    /**
     * @brief Check if a group exists within an HDF5 file.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the group to check
     * @return int              1 if the group exists, 0 otherwise
     *
     * The csc_hdf5_group_exist function checks whether a group with the specified
     * name exists within the given HDF5 location.
     */
    int csc_hdf5_group_exist(hid_t root, const char* dset_name);

    /**
     * @brief Check if a dataset exists within an HDF5 file.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset to check
     * @return int              1 if the dataset exists, 0 otherwise
     *
     * The csc_hdf5_dataset_exist function checks whether a dataset with the specified
     * name exists within the given HDF5 location.
     */
    int csc_hdf5_dataset_exist(hid_t root, const char* dset_name);

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
    int csc_hdf5_attribute_exist(hid_t root, const char* dset_name, const char* attr_name);

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
    /**
     * @brief Write an attribute of various types to a dataset (generic macro).
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] aname         Name of the attribute
     * @param[in] aval          Value to write (type determined by _Generic)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_attribute_write macro is a type-generic wrapper that automatically
     * selects the appropriate type-specific function based on the value's type.
     * Supported types: char, unsigned char, int, unsigned int, long, unsigned long, float, double.
     *
     * @see csc_hdf5_attribute_write_char
     * @see csc_hdf5_attribute_write_uchar
     * @see csc_hdf5_attribute_write_int
     * @see csc_hdf5_attribute_write_uint
     * @see csc_hdf5_attribute_write_long
     * @see csc_hdf5_attribute_write_ulong
     * @see csc_hdf5_attribute_write_float
     * @see csc_hdf5_attribute_write_double
     */
#define csc_hdf5_attribute_write(root, dset_name, aname, aval)                                                                       \
    _Generic((aval),                                                                                                                 \
        char: csc_hdf5_attribute_write_char,                                                                                         \
        unsigned char: csc_hdf5_attribute_write_uchar,                                                                               \
        int: csc_hdf5_attribute_write_int,                                                                                           \
        unsigned int: csc_hdf5_attribute_write_uint,                                                                                 \
        long: csc_hdf5_attribute_write_long,                                                                                         \
        unsigned long: csc_hdf5_attribute_write_ulong,                                                                               \
        float: csc_hdf5_attribute_write_float,                                                                                       \
        double: csc_hdf5_attribute_write_double)(root, dset_name, aname, aval)

    /**
     * @brief Read an attribute of various types from a dataset (generic macro).
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] aname         Name of the attribute
     * @param[out] aval         Pointer to store the read value (type determined by _Generic)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_attribute_read macro is a type-generic wrapper that automatically
     * selects the appropriate type-specific function based on the value's type.
     * Supported types: char*, unsigned char*, int*, unsigned int*, long*, unsigned long*, float*, double*.
     *
     * @see csc_hdf5_attribute_read_char
     * @see csc_hdf5_attribute_read_uchar
     * @see csc_hdf5_attribute_read_int
     * @see csc_hdf5_attribute_read_uint
     * @see csc_hdf5_attribute_read_long
     * @see csc_hdf5_attribute_read_ulong
     * @see csc_hdf5_attribute_read_float
     * @see csc_hdf5_attribute_read_double
     */
#define csc_hdf5_attribute_read(root, dset_name, aname, aval)                                                                        \
    _Generic((aval),                                                                                                                 \
        char*: csc_hdf5_attribute_read_char,                                                                                         \
        unsigned char*: csc_hdf5_attribute_read_uchar,                                                                               \
        int*: csc_hdf5_attribute_read_int,                                                                                           \
        unsigned int*: csc_hdf5_attribute_read_uint,                                                                                 \
        long*: csc_hdf5_attribute_read_long,                                                                                         \
        unsigned long*: csc_hdf5_attribute_read_ulong,                                                                               \
        float*: csc_hdf5_attribute_read_float,                                                                                       \
        double*: csc_hdf5_attribute_read_double)(root, dset_name, aname, aval)
#endif

    /**
     * @brief Write a character attribute to a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] aname         Name of the attribute
     * @param[in] aval          Character value to write
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_attribute_write_char(hid_t root, const char* dset_name, const char* aname, char aval);

    /**
     * @brief Write an unsigned character attribute to a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] aname         Name of the attribute
     * @param[in] aval          Unsigned character value to write
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_attribute_write_uchar(hid_t root, const char* dset_name, const char* aname, unsigned char aval);

    /**
     * @brief Write an integer attribute to a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] aname         Name of the attribute
     * @param[in] aval          Integer value to write
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_attribute_write_int(hid_t root, const char* dset_name, const char* aname, int aval);

    /**
     * @brief Write an unsigned integer attribute to a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] aname         Name of the attribute
     * @param[in] aval          Unsigned integer value to write
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_attribute_write_uint(hid_t root, const char* dset_name, const char* aname, unsigned int aval);

    /**
     * @brief Write a long integer attribute to a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] aname         Name of the attribute
     * @param[in] aval          Long integer value to write
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_attribute_write_long(hid_t root, const char* dset_name, const char* aname, long aval);

    /**
     * @brief Write an unsigned long integer attribute to a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] aname         Name of the attribute
     * @param[in] aval          Unsigned long integer value to write
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_attribute_write_ulong(hid_t root, const char* dset_name, const char* aname, unsigned long aval);

    /**
     * @brief Write a float attribute to a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] aname         Name of the attribute
     * @param[in] retval        Float value to write
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_attribute_write_float(hid_t root, const char* dset_name, const char* aname, float retval);

    /**
     * @brief Write a double attribute to a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] aname         Name of the attribute
     * @param[in] retval        Double value to write
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_attribute_write_double(hid_t root, const char* dset_name, const char* aname, double retval);

    /**
     * @brief Write a string attribute to a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] aname         Name of the attribute
     * @param[in] str           String value to write
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_attribute_write_string(hid_t root, const char* dset_name, const char* aname, char* str);

    /**
     * @brief Read a character attribute from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] aname         Name of the attribute
     * @param[out] retval       Pointer to store the read character value
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_attribute_read_char(hid_t root, const char* dset_name, const char* aname, char* retval);

    /**
     * @brief Read an unsigned character attribute from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] aname         Name of the attribute
     * @param[out] retval       Pointer to store the read unsigned character value
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_attribute_read_uchar(hid_t root, const char* dset_name, const char* aname, unsigned char* retval);

    /**
     * @brief Read an integer attribute from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] aname         Name of the attribute
     * @param[out] retval       Pointer to store the read integer value
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_attribute_read_int(hid_t root, const char* dset_name, const char* aname, int* retval);

    /**
     * @brief Read a long integer attribute from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] aname         Name of the attribute
     * @param[out] retval       Pointer to store the read long integer value
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_attribute_read_long(hid_t root, const char* dset_name, const char* aname, long* retval);

    /**
     * @brief Read an unsigned integer attribute from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] aname         Name of the attribute
     * @param[out] retval       Pointer to store the read unsigned integer value
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_attribute_read_uint(hid_t root, const char* dset_name, const char* aname, unsigned int* retval);

    /**
     * @brief Read an unsigned long integer attribute from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] aname         Name of the attribute
     * @param[out] retval       Pointer to store the read unsigned long integer value
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_attribute_read_ulong(hid_t root, const char* dset_name, const char* aname, unsigned long* retval);

    /**
     * @brief Read a float attribute from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] aname         Name of the attribute
     * @param[out] retval       Pointer to store the read float value
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_attribute_read_float(hid_t root, const char* dset_name, const char* aname, float* retval);

    /**
     * @brief Read a double attribute from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] aname         Name of the attribute
     * @param[out] retval       Pointer to store the read double value
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_attribute_read_double(hid_t root, const char* dset_name, const char* aname, double* retval);

    /**
     * @brief Read a string attribute from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] aname         Name of the attribute
     * @param[out] str          Pointer to store the read string (allocated by function)
     * @param[out] len          Pointer to store the string length
     * @return int              0 on success, negative error code otherwise
     *
     * The caller is responsible for freeing the allocated string.
     */
    int csc_hdf5_attribute_read_string(hid_t root, const char* dset_name, const char* aname, char** str, size_t* len);

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
    /**
     * @brief Read a matrix of various types from a dataset (generic macro).
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] rows         Pointer to store the number of rows
     * @param[out] cols         Pointer to store the number of columns
     * @param[out] matrix       Pointer to the matrix storage (type determined by _Generic)
     * @param[in] lda           Leading dimension of the matrix
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_read_matrix macro is a type-generic wrapper that automatically
     * selects the appropriate type-specific function based on the matrix's type.
     * Supported types: double _Complex*, float _Complex*, double*, float*, char*, short*, int*, long*.
     *
     * @see csc_hdf5_read_double_matrix
     * @see csc_hdf5_read_float_matrix
     * @see csc_hdf5_read_double_complex_matrix
     * @see csc_hdf5_read_float_complex_matrix
     * @see csc_hdf5_read_char_matrix
     * @see csc_hdf5_read_short_matrix
     * @see csc_hdf5_read_int_matrix
     * @see csc_hdf5_read_long_matrix
     */
#define csc_hdf5_read_matrix(root, dset_name, rows, cols, matrix, lda, opts)                                                         \
    _Generic((matrix),                                                                                                               \
        double _Complex*: csc_hdf5_read_double_complex_matrix,                                                                       \
        float _Complex*: csc_hdf5_read_float_complex_matrix,                                                                         \
        double*: csc_hdf5_read_double_matrix,                                                                                        \
        float*: csc_hdf5_read_float_matrix,                                                                                          \
        char*: csc_hdf5_read_char_matrix,                                                                                            \
        short*: csc_hdf5_read_short_matrix,                                                                                          \
        int*: csc_hdf5_read_int_matrix,                                                                                              \
        long*: csc_hdf5_read_long_matrix)(root, dset_name, rows, cols, matrix, lda, opts)

    /**
     * @brief Read a vector of various types from a dataset (generic macro).
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] rows         Pointer to store the number of elements
     * @param[out] matrix       Pointer to the vector storage (type determined by _Generic)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_read_vector macro is a type-generic wrapper that automatically
     * selects the appropriate type-specific function based on the vector's type.
     * Supported types: double _Complex*, float _Complex*, double*, float*, char*, short*, int*, long*.
     *
     * @see csc_hdf5_read_double_vector
     * @see csc_hdf5_read_float_vector
     * @see csc_hdf5_read_double_complex_vector
     * @see csc_hdf5_read_float_complex_vector
     * @see csc_hdf5_read_char_vector
     * @see csc_hdf5_read_short_vector
     * @see csc_hdf5_read_int_vector
     * @see csc_hdf5_read_long_vector
     */
#define csc_hdf5_read_vector(root, dset_name, rows, matrix, opts)                                                                    \
    _Generic((matrix),                                                                                                               \
        double _Complex*: csc_hdf5_read_double_complex_vector,                                                                       \
        float _Complex*: csc_hdf5_read_float_complex_vector,                                                                         \
        double*: csc_hdf5_read_double_vector,                                                                                        \
        float*: csc_hdf5_read_float_vector,                                                                                          \
        char*: csc_hdf5_read_char_vector,                                                                                            \
        short*: csc_hdf5_read_short_vector,                                                                                          \
        int*: csc_hdf5_read_int_vector,                                                                                              \
        long*: csc_hdf5_read_long_vector)(root, dset_name, rows, matrix, opts)

#endif

    /**
     * @brief Read a double precision matrix from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows          Number of rows
     * @param[in] cols          Number of columns
     * @param[out] matrix       Pointer to the matrix storage (Fortran column-major order)
     * @param[in] lda           Leading dimension of the matrix
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_read_double_matrix(hid_t root, const char* dset_name, size_t rows, size_t cols, double* matrix, size_t lda,
                                    csc_hdf5_options_t* opts);

    /**
     * @brief Read a single precision float matrix from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows          Number of rows
     * @param[in] cols          Number of columns
     * @param[out] matrix       Pointer to the matrix storage (Fortran column-major order)
     * @param[in] lda           Leading dimension of the matrix
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_read_float_matrix(hid_t root, const char* dset_name, size_t rows, size_t cols, float* matrix, size_t lda,
                                   csc_hdf5_options_t* opts);

    /**
     * @brief Read a double precision complex matrix from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows          Number of rows
     * @param[in] cols          Number of columns
     * @param[out] matrix       Pointer to the matrix storage (Fortran column-major order)
     * @param[in] lda           Leading dimension of the matrix
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_read_double_complex_matrix(hid_t root, const char* dset_name, size_t rows, size_t cols, double complex* matrix,
                                            size_t lda, csc_hdf5_options_t* opts);

    /**
     * @brief Read a single precision complex matrix from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows          Number of rows
     * @param[in] cols          Number of columns
     * @param[out] matrix       Pointer to the matrix storage (Fortran column-major order)
     * @param[in] lda           Leading dimension of the matrix
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_read_float_complex_matrix(hid_t root, const char* dset_name, size_t rows, size_t cols, float complex* matrix,
                                           size_t lda, csc_hdf5_options_t* opts);

    /**
     * @brief Read a character matrix from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows          Number of rows
     * @param[in] cols          Number of columns
     * @param[out] matrix       Pointer to the matrix storage
     * @param[in] lda           Leading dimension of the matrix
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_read_char_matrix(hid_t root, const char* dset_name, size_t rows, size_t cols, char* matrix, size_t lda,
                                  csc_hdf5_options_t* opts);

    /**
     * @brief Read a short integer matrix from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows          Number of rows
     * @param[in] cols          Number of columns
     * @param[out] matrix       Pointer to the matrix storage
     * @param[in] lda           Leading dimension of the matrix
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_read_short_matrix(hid_t root, const char* dset_name, size_t rows, size_t cols, short* matrix, size_t lda,
                                   csc_hdf5_options_t* opts);

    /**
     * @brief Read an integer matrix from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows          Number of rows
     * @param[in] cols          Number of columns
     * @param[out] matrix       Pointer to the matrix storage
     * @param[in] lda           Leading dimension of the matrix
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_read_int_matrix(hid_t root, const char* dset_name, size_t rows, size_t cols, int* matrix, size_t lda,
                                 csc_hdf5_options_t* opts);

    /**
     * @brief Read a long integer matrix from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows          Number of rows
     * @param[in] cols          Number of columns
     * @param[out] matrix       Pointer to the matrix storage
     * @param[in] lda           Leading dimension of the matrix
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_read_long_matrix(hid_t root, const char* dset_name, size_t rows, size_t cols, long* matrix, size_t lda,
                                  csc_hdf5_options_t* opts);

    /**
     * @brief Read a double precision vector from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows          Number of elements
     * @param[out] matrix       Pointer to the vector storage
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_read_double_vector(hid_t root, const char* dset_name, size_t rows, double* matrix, csc_hdf5_options_t* opts);

    /**
     * @brief Read a single precision float vector from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows          Number of elements
     * @param[out] matrix       Pointer to the vector storage
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_read_float_vector(hid_t root, const char* dset_name, size_t rows, float* matrix, csc_hdf5_options_t* opts);

    /**
     * @brief Read a double precision complex vector from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows          Number of elements
     * @param[out] matrix       Pointer to the vector storage
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_read_double_complex_vector(hid_t root, const char* dset_name, size_t rows, double complex* matrix,
                                            csc_hdf5_options_t* opts);

    /**
     * @brief Read a single precision complex vector from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows          Number of elements
     * @param[out] matrix       Pointer to the vector storage
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_read_float_complex_vector(hid_t root, const char* dset_name, size_t rows, float complex* matrix,
                                           csc_hdf5_options_t* opts);

    /**
     * @brief Read a character vector from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows          Number of elements
     * @param[out] matrix       Pointer to the vector storage
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_read_char_vector(hid_t root, const char* dset_name, size_t rows, char* matrix, csc_hdf5_options_t* opts);

    /**
     * @brief Read a short integer vector from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows          Number of elements
     * @param[out] matrix       Pointer to the vector storage
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_read_short_vector(hid_t root, const char* dset_name, size_t rows, short* matrix, csc_hdf5_options_t* opts);

    /**
     * @brief Read an integer vector from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows          Number of elements
     * @param[out] matrix       Pointer to the vector storage
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_read_int_vector(hid_t root, const char* dset_name, size_t rows, int* matrix, csc_hdf5_options_t* opts);

    /**
     * @brief Read a long integer vector from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows          Number of elements
     * @param[out] matrix       Pointer to the vector storage
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_read_long_vector(hid_t root, const char* dset_name, size_t rows, long* matrix, csc_hdf5_options_t* opts);

    /**
     * @brief Read a string from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] str          Pointer to store the read string (allocated by function)
     * @param[out] len          Pointer to store the string length
     * @return int              0 on success, negative error code otherwise
     *
     * The caller is responsible for freeing the allocated string.
     */
    int csc_hdf5_read_string(hid_t root, const char* dset_name, char** str, size_t* len);

    /**
     * @brief Write a string to a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] str           String value to write
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_write_string(hid_t root, const char* dset_name, const char* str);

    /**
     * @brief Write a scalar value of arbitrary type to a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] dtype         HDF5 datatype identifier
     * @param[in] val           Pointer to the value to write
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_write_scalar function is a low-level function that allows writing
     * arbitrary scalar values to a dataset by specifying the HDF5 datatype directly.
     */
    int csc_hdf5_write_scalar(hid_t root, const char* dset_name, hid_t dtype, void* val);

    /**
     * @brief Write an integer value to a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] val           Integer value to write
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_write_int(hid_t root, const char* dset_name, int val);

    /**
     * @brief Write an unsigned integer value to a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] val           Unsigned integer value to write
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_write_uint(hid_t root, const char* dset_name, unsigned int val);

    /**
     * @brief Write a long integer value to a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] val           Long integer value to write
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_write_long(hid_t root, const char* dset_name, long val);

    /**
     * @brief Write an unsigned long integer value to a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] val           Unsigned long integer value to write
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_write_ulong(hid_t root, const char* dset_name, unsigned long val);

    /**
     * @brief Write a float value to a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] val           Float value to write
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_write_float(hid_t root, const char* dset_name, float val);

    /**
     * @brief Write a double value to a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] val           Double value to write
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_write_double(hid_t root, const char* dset_name, double val);

    /**
     * @brief Read a scalar value of arbitrary type from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] dtype         HDF5 datatype identifier
     * @param[out] val          Pointer to store the read value
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_read_scalar function is a low-level function that allows reading
     * arbitrary scalar values from a dataset by specifying the HDF5 datatype directly.
     */
    int csc_hdf5_read_scalar(hid_t root, const char* dset_name, hid_t dtype, void* val);

    /**
     * @brief Read an integer value from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] val          Pointer to store the read integer value
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_read_int(hid_t root, const char* dset_name, int* val);

    /**
     * @brief Read an unsigned integer value from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] val          Pointer to store the read unsigned integer value
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_read_uint(hid_t root, const char* dset_name, unsigned int* val);

    /**
     * @brief Read a long integer value from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] val          Pointer to store the read long integer value
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_read_long(hid_t root, const char* dset_name, long* val);

    /**
     * @brief Read an unsigned long integer value from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] val          Pointer to store the read unsigned long integer value
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_read_ulong(hid_t root, const char* dset_name, unsigned long* val);

    /**
     * @brief Read a float value from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] val          Pointer to store the read float value
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_read_float(hid_t root, const char* dset_name, float* val);

    /**
     * @brief Read a double value from a dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] val          Pointer to store the read double value
     * @return int              0 on success, negative error code otherwise
     */
    int csc_hdf5_read_double(hid_t root, const char* dset_name, double* val);

    /**
     * @brief Check if a dataset contains a sparse matrix in CSR format.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @return int              1 if sparse CSR matrix, 0 otherwise
     *
     * The csc_hdf5_is_sparse function checks the content type attribute of the
     * dataset to determine if it represents a sparse matrix in CSR format.
     */
    int csc_hdf5_is_sparse(hid_t root, const char* dset_name);

    /**
     * @brief Check if a dataset was created in Octave format.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @return int              1 if Octave format, 0 otherwise
     *
     * The csc_hdf5_is_octave function checks whether the dataset contains
     * attributes specific to Octave's HDF5 format.
     */
    int csc_hdf5_is_octave(hid_t root, const char* dset_name);

    /**
     * @brief Create a HDF5 compound type to represent a complex number.
     * @param[in] root          HDF5 file or group identifier
     * @return hid_t            The data type id or a negative value in case of an error.
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
    int csc_hdf5_is_vector(hid_t root, const char* dset_name);

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
    int csc_hdf5_is_matrix(hid_t root, const char* dset_name);

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
    int csc_hdf5_is_spcsr(hid_t root, const char* dset_name);

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
    int csc_hdf5_is_spcsc(hid_t root, const char* dset_name);

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
    int csc_hdf5_is_spcoo(hid_t root, const char* dset_name);

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
    int csc_hdf5_is_real(hid_t root, const char* dset_name);

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
    int csc_hdf5_is_real_single(hid_t root, const char* dset_name);

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
    int csc_hdf5_is_complex(hid_t root, const char* dset_name);

    /**
     * @brief Return a human readable representation of the datatype's class.
     * @param[in]  d    type id of the datatype.
     * @return a string containing the human readable representation of the datatype's class.
     *
     * The csc_hdf5_get_classname function returns a human readable representation of the H5Tget_class
     * function.
     */
    char* csc_hdf5_get_classname(hid_t d);

    /**
     * @brief Print the native of a given data type.
     * @param[in] dtype_id ID of the data type.
     * @param[in] direction Search direction for the native type.
     *
     * The csc_hdf5_print_native_type function search for the first fitting
     * native data type using H5Tget_native_type. Therefore a search direction
     * can be specified. This is either H5T_DIR_ASCEND or H5T_DIR_DESCEND.
     */
    void csc_hdf5_print_native_type(hid_t dtype_id, H5T_direction_t direction);

    /**
     * @brief Enable or disable compression of datasets
     * @param[in] comp  Compression Type.
     *
     * The csc_hdf5_set_compression function enables or disables the compression of
     * the data sets.
     */
    void csc_hdf5_set_compression(csc_hdf5_compression_t comp);

    /**
     * @brief Get the current compression type.
     * @return csc_hdf5_compression_t   The current compression type
     *
     * The csc_hdf5_get_compression function returns the currently configured
     * compression algorithm for datasets.
     */
    csc_hdf5_compression_t csc_hdf5_get_compression(void);

    /**
     * @brief Get the current compression type as integer.
     * @return int    The current compression type as integer value
     *
     * The csc_hdf5_get_compression_int function returns the currently configured
     * compression algorithm for datasets as an integer value.
     */
    int csc_hdf5_get_compression_int(void);

    /**
     * @brief Check if a dataset exists in an HDF5 file.
     * @param[in] h5file        HDF5 file identifier
     * @param[in] name          Name of the dataset to check
     * @return int              1 if the dataset exists, 0 otherwise
     *
     * The csc_hdf5_exist function checks whether a dataset with the specified
     * name exists within the given HDF5 file.
     */
    int csc_hdf5_exist(hid_t h5file, const char* name);

    /**
     * @brief Create a dataset in an HDF5 file with compression support.
     * @param[in]   loc         root location of the new dataset
     * @param[in]   dset_name   name of the new dataset
     * @param[in]   rank        number of dimension of the dataset
     * @param[in]   dims        array containing the dimensions.
     * @param[in]   tid         id of the data type
     * @param[in]   data        pointer to the data
     * @return zero on success or a negative error code
     *
     * The csc_hdf5_create_dataset function creates a dataset using low-level HDF5 functions
     * with compression support. The compression is enabled based on the value set by
     * \ref csc_hdf5_set_compression.
     *
     */
    int csc_hdf5_create_dataset(hid_t loc, const char* dset_name, int rank, const hsize_t* dims, hid_t tid, const void* data);

    /**
     * @brief Read a dataset from an HDF5 file (replacement for H5LTread_dataset)
     * @param[in]   loc       HDF5 location identifier (file or group)
     * @param[in]   dset_name Name of the dataset to read
     * @param[in]   tid       Data type identifier
     * @param[out]  data      Buffer to store the read data
     * @return 0 on success, negative value on failure
     */
    int csc_hdf5_read_dataset(hid_t loc, const char* dset_name, hid_t tid, void* data);

    /**
     * @brief Return the length of a vector in an HDF5 storage.
     * @param[in]   root        root id of the dataset
     * @param[in]   dset_name   name of the dataset.
     * @return the length of the vector or a negative error code.
     *
     * The csc_hdf5_vector_len function returns the length of a vector stored
     * in a HDF5 file and addressed by root and dset_name.
     */
    long csc_hdf5_vector_len(hid_t root, const char* dset_name);

    /**
     * @brief Get the numeric field type of a vector dataset.
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @return csc_hdf5_field_t The field type of the vector
     *
     * The csc_hdf5_vector_field function reads the field type attribute from
     * a vector dataset.
     */
    csc_hdf5_field_t csc_hdf5_vector_field(hid_t root, const char* dset_name);

    /**
     * @brief Read a vector from an HDF5 dataset.
     * @param[in]  field        Number field of the vector
     * @param[in]  root         root id of the dataset
     * @param[in]  dset_name    name of the dataset
     * @param[out] vector       preallocated vector
     * @return zero on success or a non zero error code otherwise
     *
     * The csc_hdf5_vector_read function read a vector from the specified dataset. The output
     * vector needs to be a preallocated array of the correct type and dimension. The dimension
     * of the vector can be retrieved using \ref csc_hdf5_vector_len and the data type is either
     * double or double complex and can be checked using \ref csc_hdf5_is_real or \ref csc_hdf5_is_complex.
     */
    int csc_hdf5_vector_read(csc_hdf5_field_t field, hid_t root, const char* dset_name, void* vector)
        DEPRECATE_MSG("Use csc_hdf5_read_vector instead.");

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
    int csc_hdf5_vector_write(csc_hdf5_field_t field, hid_t root, const char* dset_name, size_t len, void* vector);

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
    int csc_hdf5_matrix_write_real(hid_t root, const char* dset_name, size_t rows, size_t cols, size_t ld, double* matrix);
    int csc_hdf5_matrix_write_real_single(hid_t root, const char* dset_name, size_t rows, size_t cols, size_t ld, float* matrix);

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
    int csc_hdf5_matrix_write_complex(hid_t root, const char* dset_name, size_t rows, size_t cols, size_t ld, dcomplex* matrix);

    int csc_hdf5_matrix_read_real(hid_t root, const char* dset_name, size_t rows, size_t cols, size_t ld, double* matrix);
    int csc_hdf5_matrix_read_real_single(hid_t root, const char* dset_name, size_t rows, size_t cols, size_t ld, float* matrix);

    int csc_hdf5_matrix_read_complex(hid_t root, const char* dset_name, size_t rows, size_t cols, size_t ld, dcomplex* matrix);

    /**
     * @brief Write a sparse matrix to an HDF5 dataset.
     * @param[in] type              Sparse matrix type (CSR, CSC, COO)
     * @param[in] number_field      Numeric field type of the values
     * @param[in] integer_field     Numeric field type of the indices
     * @param[in] root              HDF5 file or group identifier
     * @param[in] dset_name         Name of the dataset
     * @param[in] rows              Number of rows
     * @param[in] cols              Number of columns
     * @param[in] nnz               Number of non-zero elements
     * @param[in] rowptr            Row pointer array (CSR) or row indices (COO)
     * @param[in] colptr            Column index array (CSR) or column indices (COO)
     * @param[in] values            Array of non-zero values
     * @return int                  0 on success, negative error code otherwise
     *
     * The csc_hdf5_sparse_write function writes a sparse matrix in the specified
     * format to an HDF5 dataset. The matrix data and indices are stored according
     * to the specified format.
     */
    int csc_hdf5_sparse_write(csc_hdf5_content_t type, csc_hdf5_field_t number_field, csc_hdf5_field_t integer_field, hid_t root,
                              const char* dset_name, size_t rows, size_t cols, size_t nnz, void* rowptr, void* colptr, void* values);

    /**
     * @brief Read a sparse matrix from an HDF5 dataset.
     * @param[out] type             Pointer to store the sparse matrix type
     * @param[out] number_field     Pointer to store the numeric field type
     * @param[out] integer_field    Pointer to store the integer field type
     * @param[in] root              HDF5 file or group identifier
     * @param[in] dset_name         Name of the dataset
     * @param[out] rows             Pointer to store the number of rows
     * @param[out] cols             Pointer to store the number of columns
     * @param[out] nnz              Pointer to store the number of non-zero elements
     * @param[out] rowptr           Row pointer array (CSR) or row indices (COO)
     * @param[out] colptr           Column index array (CSR) or column indices (COO)
     * @param[out] values           Array of non-zero values
     * @return int                  0 on success, negative error code otherwise
     *
     * The csc_hdf5_sparse_read function reads a sparse matrix from an HDF5 dataset.
     * The caller must provide preallocated arrays for the matrix data and indices.
     */
    int csc_hdf5_sparse_read(csc_hdf5_content_t* type, csc_hdf5_field_t* number_field, csc_hdf5_field_t integer_field, hid_t root,
                             const char* dset_name, size_t* rows, size_t* cols, size_t* nnz, void* rowptr, void* colptr,
                             void* values);

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
    /**
     * @brief Write a matrix of various types to a dataset (generic macro).
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows          Number of rows
     * @param[in] cols          Number of columns
     * @param[in] matrix        Pointer to the matrix storage (type determined by _Generic)
     * @param[in] lda           Leading dimension of the matrix
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_write_matrix macro is a type-generic wrapper that automatically
     * selects the appropriate type-specific function based on the matrix's type.
     * Supported types: double _Complex*, float _Complex*, double*, float*.
     *
     * @see csc_hdf5_write_double_matrix
     * @see csc_hdf5_write_float_matrix
     * @see csc_hdf5_write_double_complex_matrix
     * @see csc_hdf5_write_float_complex_matrix
     */
#define csc_hdf5_write_matrix(root, dset_name, rows, cols, matrix, lda, opts)                                                        \
    _Generic((matrix),                                                                                                               \
        double _Complex*: csc_hdf5_write_double_complex_matrix,                                                                      \
        float _Complex*: csc_hdf5_write_float_complex_matrix,                                                                        \
        double*: csc_hdf5_write_double_matrix,                                                                                       \
        float*: csc_hdf5_write_float_matrix)(root, dset_name, rows, cols, matrix, lda, opts)

    /**
     * @brief Write a vector of various types to a dataset (generic macro).
     * @param[in] root          HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows          Number of elements
     * @param[in] matrix        Pointer to the vector storage (type determined by _Generic)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_write_vector macro is a type-generic wrapper that automatically
     * selects the appropriate type-specific function based on the vector's type.
     * Supported types: double _Complex*, float _Complex*, double*, float*.
     *
     * @see csc_hdf5_write_double_vector
     * @see csc_hdf5_write_float_vector
     * @see csc_hdf5_write_double_complex_vector
     * @see csc_hdf5_write_float_complex_vector
     */
#define csc_hdf5_write_vector(root, dset_name, rows, matrix, opts)                                                                   \
    _Generic((matrix),                                                                                                               \
        double _Complex*: csc_hdf5_write_double_complex_vector,                                                                      \
        float _Complex*: csc_hdf5_write_float_complex_vector,                                                                        \
        double*: csc_hdf5_write_double_vector,                                                                                       \
        float*: csc_hdf5_write_float_vector)(root, dset_name, rows, matrix, opts)
#endif

    /**
     * @brief Write a double precision matrix to a dataset.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows      Number of rows of the matrix
     * @param[in] cols      Number of columns of the matrix
     * @param[in] ld        Leading (row) dimension of the matrix
     * @param[in] matrix        Pointer to the matrix (Fortran column-major order)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return zero on success or a non zero error code otherwise
     *
     * The csc_hdf5_write_double_matrix function writes a double precision matrix into
     * the specified dataset. The matrix must be given in Fortran storage scheme and
     * it is stored as transpose of it self in the dataset. The reason behind that is
     * that normally all numerical algorithms require the Fortran storage but HDF5 stores
     * multidimensional arrays in C style.
     * If the compression is set using \ref csc_hdf5_set_compression
     * the data will be compressed during the write operation.
     */
    int csc_hdf5_write_double_matrix(hid_t root, const char* dset_name, size_t rows, size_t cols, double* matrix, size_t lda,
                                     csc_hdf5_options_t* opts);

    /**
     * @brief Write a single precision float matrix to a dataset.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows      Number of rows of the matrix
     * @param[in] cols      Number of columns of the matrix
     * @param[in] ld        Leading (row) dimension of the matrix
     * @param[in] matrix        Pointer to the matrix (Fortran column-major order)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return zero on success or a non zero error code otherwise
     *
     * The csc_hdf5_write_float_matrix function writes a single precision float matrix into
     * the specified dataset. The matrix must be given in Fortran storage scheme and
     * it is stored as transpose of it self in the dataset. The reason behind that is
     * that normally all numerical algorithms require the Fortran storage but HDF5 stores
     * multidimensional arrays in C style.
     * If the compression is set using \ref csc_hdf5_set_compression
     * the data will be compressed during the write operation.
     */
    int csc_hdf5_write_float_matrix(hid_t root, const char* dset_name, size_t rows, size_t cols, float* matrix, size_t lda,
                                    csc_hdf5_options_t* opts);

    /**
     * @brief Write a double precision complex matrix to a dataset.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows      Number of rows of the matrix
     * @param[in] cols      Number of columns of the matrix
     * @param[in] ld        Leading (row) dimension of the matrix
     * @param[in] matrix        Pointer to the matrix (Fortran column-major order)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return zero on success or a non zero error code otherwise
     *
     * The csc_hdf5_write_double_complex_matrix function writes a double precision complex matrix into
     * the specified dataset. The matrix must be given in Fortran storage scheme and
     * it is stored as transpose of it self in the dataset. The reason behind that is
     * that normally all numerical algorithms require the Fortran storage but HDF5 stores
     * multidimensional arrays in C style.
     * If the compression is set using \ref csc_hdf5_set_compression
     * the data will be compressed during the write operation.
     */
    int csc_hdf5_write_double_complex_matrix(hid_t root, const char* dset_name, size_t rows, size_t cols, dcomplex* matrix,
                                             size_t lda, csc_hdf5_options_t* opts);

    /**
     * @brief Write a single precision complex matrix to a dataset.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows      Number of rows of the matrix
     * @param[in] cols      Number of columns of the matrix
     * @param[in] ld        Leading (row) dimension of the matrix
     * @param[in] matrix        Pointer to the matrix (Fortran column-major order)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return zero on success or a non zero error code otherwise
     *
     * The csc_hdf5_write_float_complex_matrix function writes a single precision complex matrix into
     * the specified dataset. The matrix must be given in Fortran storage scheme and
     * it is stored as transpose of it self in the dataset. The reason behind that is
     * that normally all numerical algorithms require the Fortran storage but HDF5 stores
     * multidimensional arrays in C style.
     * If the compression is set using \ref csc_hdf5_set_compression
     * the data will be compressed during the write operation.
     */
    int csc_hdf5_write_float_complex_matrix(hid_t root, const char* dset_name, size_t rows, size_t cols, fcomplex* matrix, size_t lda,
                                            csc_hdf5_options_t* opts);

    /**
     * @brief Write a double precision vector to a dataset.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows      Number of elements in the vector
     * @param[in] matrix        Pointer to the vector
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return zero on success or a non zero error code otherwise
     *
     * The csc_hdf5_write_double_vector function writes a double precision vector into
     * the specified dataset.
     * If the compression is set using \ref csc_hdf5_set_compression
     * the data will be compressed during the write operation.
     */
    int csc_hdf5_write_double_vector(hid_t root, const char* dset_name, size_t rows, double* matrix, csc_hdf5_options_t* opts);

    /**
     * @brief Write a single precision float vector to a dataset.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows      Number of elements in the vector
     * @param[in] matrix        Pointer to the vector
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return zero on success or a non zero error code otherwise
     *
     * The csc_hdf5_write_float_vector function writes a single precision float vector into
     * the specified dataset.
     * If the compression is set using \ref csc_hdf5_set_compression
     * the data will be compressed during the write operation.
     */
    int csc_hdf5_write_float_vector(hid_t root, const char* dset_name, size_t rows, float* matrix, csc_hdf5_options_t* opts);

    /**
     * @brief Write a double precision complex vector to a dataset.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows      Number of elements in the vector
     * @param[in] matrix        Pointer to the vector
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return zero on success or a non zero error code otherwise
     *
     * The csc_hdf5_write_double_complex_vector function writes a double precision complex vector into
     * the specified dataset.
     * If the compression is set using \ref csc_hdf5_set_compression
     * the data will be compressed during the write operation.
     */
    int csc_hdf5_write_double_complex_vector(hid_t root, const char* dset_name, size_t rows, double complex* matrix,
                                             csc_hdf5_options_t* opts);

    /**
     * @brief Write a single precision complex vector to a dataset.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows      Number of elements in the vector
     * @param[in] matrix        Pointer to the vector
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return zero on success or a non zero error code otherwise
     *
     * The csc_hdf5_write_float_complex_vector function writes a single precision complex vector into
     * the specified dataset.
     * If the compression is set using \ref csc_hdf5_set_compression
     * the data will be compressed during the write operation.
     */
    int csc_hdf5_write_float_complex_vector(hid_t root, const char* dset_name, size_t rows, float complex* matrix,
                                            csc_hdf5_options_t* opts);

    /**
     * @brief Write a sparse matrix with integer indices (int) and double values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows      Number of rows
     * @param[in] cols      Number of columns
     * @param[in] nnz       Number of non-zero elements
     * @param[in] rowptr        Row pointer array (length = rows+1)
     * @param[in] colptr        Column index array (length = nnz)
     * @param[in] data      Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_write_double_sparse_matrix_int function writes a sparse matrix
     * in CSR format using int for indices and double for values.
     */
    int csc_hdf5_write_double_sparse_matrix_int(hid_t root, const char* dset_name, size_t rows, size_t cols, size_t nnz, int* rowptr,
                                                int* colptr, double* data, csc_hdf5_options_t* opts);

    /**
     * @brief Write a sparse matrix with unsigned int indices and double values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows      Number of rows
     * @param[in] cols      Number of columns
     * @param[in] nnz       Number of non-zero elements
     * @param[in] rowptr        Row pointer array (length = rows+1)
     * @param[in] colptr        Column index array (length = nnz)
     * @param[in] data      Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_write_double_sparse_matrix_uint function writes a sparse matrix
     * in CSR format using unsigned int for indices and double for values.
     */
    int csc_hdf5_write_double_sparse_matrix_uint(hid_t root, const char* dset_name, size_t rows, size_t cols, size_t nnz,
                                                 unsigned int* rowptr, unsigned int* colptr, double* data, csc_hdf5_options_t* opts);

    /**
     * @brief Write a sparse matrix with long indices and double values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows      Number of rows
     * @param[in] cols      Number of columns
     * @param[in] nnz       Number of non-zero elements
     * @param[in] rowptr        Row pointer array (length = rows+1)
     * @param[in] colptr        Column index array (length = nnz)
     * @param[in] data      Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_write_double_sparse_matrix_long function writes a sparse matrix
     * in CSR format using long for indices and double for values.
     */
    int csc_hdf5_write_double_sparse_matrix_long(hid_t root, const char* dset_name, size_t rows, size_t cols, size_t nnz,
                                                 long* rowptr, long* colptr, double* data, csc_hdf5_options_t* opts);

    /**
     * @brief Write a sparse matrix with unsigned long indices and double values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows      Number of rows
     * @param[in] cols      Number of columns
     * @param[in] nnz       Number of non-zero elements
     * @param[in] rowptr        Row pointer array (length = rows+1)
     * @param[in] colptr        Column index array (length = nnz)
     * @param[in] data      Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_write_double_sparse_matrix_ulong function writes a sparse matrix
     * in CSR format using unsigned long for indices and double for values.
     */
    int csc_hdf5_write_double_sparse_matrix_ulong(hid_t root, const char* dset_name, size_t rows, size_t cols, size_t nnz,
                                                  unsigned long* rowptr, unsigned long* colptr, double* data,
                                                  csc_hdf5_options_t* opts);

    /**
     * @brief Write a sparse matrix with integer indices (int) and float values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows      Number of rows
     * @param[in] cols      Number of columns
     * @param[in] nnz       Number of non-zero elements
     * @param[in] rowptr        Row pointer array (length = rows+1)
     * @param[in] colptr        Column index array (length = nnz)
     * @param[in] data      Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_write_float_sparse_matrix_int function writes a sparse matrix
     * in CSR format using int for indices and float for values.
     */
    int csc_hdf5_write_float_sparse_matrix_int(hid_t root, const char* dset_name, size_t rows, size_t cols, size_t nnz, int* rowptr,
                                               int* colptr, float* data, csc_hdf5_options_t* opts);

    /**
     * @brief Write a sparse matrix with unsigned int indices and float values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows      Number of rows
     * @param[in] cols      Number of columns
     * @param[in] nnz       Number of non-zero elements
     * @param[in] rowptr        Row pointer array (length = rows+1)
     * @param[in] colptr        Column index array (length = nnz)
     * @param[in] data      Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_write_float_sparse_matrix_uint function writes a sparse matrix
     * in CSR format using unsigned int for indices and float for values.
     */
    int csc_hdf5_write_float_sparse_matrix_uint(hid_t root, const char* dset_name, size_t rows, size_t cols, size_t nnz,
                                                unsigned int* rowptr, unsigned int* colptr, float* data, csc_hdf5_options_t* opts);

    /**
     * @brief Write a sparse matrix with long indices and float values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows      Number of rows
     * @param[in] cols      Number of columns
     * @param[in] nnz       Number of non-zero elements
     * @param[in] rowptr        Row pointer array (length = rows+1)
     * @param[in] colptr        Column index array (length = nnz)
     * @param[in] data      Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_write_float_sparse_matrix_long function writes a sparse matrix
     * in CSR format using long for indices and float for values.
     */
    int csc_hdf5_write_float_sparse_matrix_long(hid_t root, const char* dset_name, size_t rows, size_t cols, size_t nnz, long* rowptr,
                                                long* colptr, float* data, csc_hdf5_options_t* opts);

    /**
     * @brief Write a sparse matrix with unsigned long indices and float values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows      Number of rows
     * @param[in] cols      Number of columns
     * @param[in] nnz       Number of non-zero elements
     * @param[in] rowptr        Row pointer array (length = rows+1)
     * @param[in] colptr        Column index array (length = nnz)
     * @param[in] data      Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_write_float_sparse_matrix_ulong function writes a sparse matrix
     * in CSR format using unsigned long for indices and float for values.
     */
    int csc_hdf5_write_float_sparse_matrix_ulong(hid_t root, const char* dset_name, size_t rows, size_t cols, size_t nnz,
                                                 unsigned long* rowptr, unsigned long* colptr, float* data, csc_hdf5_options_t* opts);

    /**
     * @brief Write a sparse matrix with integer indices (int) and double complex values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows      Number of rows
     * @param[in] cols      Number of columns
     * @param[in] nnz       Number of non-zero elements
     * @param[in] rowptr        Row pointer array (length = rows+1)
     * @param[in] colptr        Column index array (length = nnz)
     * @param[in] data      Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_write_double_complex_sparse_matrix_int function writes a sparse matrix
     * in CSR format using int for indices and double complex for values.
     */
    int csc_hdf5_write_double_complex_sparse_matrix_int(hid_t root, const char* dset_name, size_t rows, size_t cols, size_t nnz,
                                                        int* rowptr, int* colptr, double complex* data, csc_hdf5_options_t* opts);

    /**
     * @brief Write a sparse matrix with unsigned int indices and double complex values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows      Number of rows
     * @param[in] cols      Number of columns
     * @param[in] nnz       Number of non-zero elements
     * @param[in] rowptr        Row pointer array (length = rows+1)
     * @param[in] colptr        Column index array (length = nnz)
     * @param[in] data      Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_write_double_complex_sparse_matrix_uint function writes a sparse matrix
     * in CSR format using unsigned int for indices and double complex for values.
     */
    int csc_hdf5_write_double_complex_sparse_matrix_uint(hid_t root, const char* dset_name, size_t rows, size_t cols, size_t nnz,
                                                         unsigned int* rowptr, unsigned int* colptr, double complex* data,
                                                         csc_hdf5_options_t* opts);

    /**
     * @brief Write a sparse matrix with long indices and double complex values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows      Number of rows
     * @param[in] cols      Number of columns
     * @param[in] nnz       Number of non-zero elements
     * @param[in] rowptr        Row pointer array (length = rows+1)
     * @param[in] colptr        Column index array (length = nnz)
     * @param[in] data      Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_write_double_complex_sparse_matrix_long function writes a sparse matrix
     * in CSR format using long for indices and double complex for values.
     */
    int csc_hdf5_write_double_complex_sparse_matrix_long(hid_t root, const char* dset_name, size_t rows, size_t cols, size_t nnz,
                                                         long* rowptr, long* colptr, double complex* data, csc_hdf5_options_t* opts);

    /**
     * @brief Write a sparse matrix with unsigned long indices and double complex values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows      Number of rows
     * @param[in] cols      Number of columns
     * @param[in] nnz       Number of non-zero elements
     * @param[in] rowptr        Row pointer array (length = rows+1)
     * @param[in] colptr        Column index array (length = nnz)
     * @param[in] data      Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_write_double_complex_sparse_matrix_ulong function writes a sparse matrix
     * in CSR format using unsigned long for indices and double complex for values.
     */
    int csc_hdf5_write_double_complex_sparse_matrix_ulong(hid_t root, const char* dset_name, size_t rows, size_t cols, size_t nnz,
                                                          unsigned long* rowptr, unsigned long* colptr, double complex* data,
                                                          csc_hdf5_options_t* opts);

    /**
     * @brief Write a sparse matrix with integer indices (int) and float complex values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows      Number of rows
     * @param[in] cols      Number of columns
     * @param[in] nnz       Number of non-zero elements
     * @param[in] rowptr        Row pointer array (length = rows+1)
     * @param[in] colptr        Column index array (length = nnz)
     * @param[in] data      Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_write_float_complex_sparse_matrix_int function writes a sparse matrix
     * in CSR format using int for indices and float complex for values.
     */
    int csc_hdf5_write_float_complex_sparse_matrix_int(hid_t root, const char* dset_name, size_t rows, size_t cols, size_t nnz,
                                                       int* rowptr, int* colptr, float complex* data, csc_hdf5_options_t* opts);

    /**
     * @brief Write a sparse matrix with unsigned int indices and float complex values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows      Number of rows
     * @param[in] cols      Number of columns
     * @param[in] nnz       Number of non-zero elements
     * @param[in] rowptr        Row pointer array (length = rows+1)
     * @param[in] colptr        Column index array (length = nnz)
     * @param[in] data      Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_write_float_complex_sparse_matrix_uint function writes a sparse matrix
     * in CSR format using unsigned int for indices and float complex for values.
     */
    int csc_hdf5_write_float_complex_sparse_matrix_uint(hid_t root, const char* dset_name, size_t rows, size_t cols, size_t nnz,
                                                        unsigned int* rowptr, unsigned int* colptr, float complex* data,
                                                        csc_hdf5_options_t* opts);

    /**
     * @brief Write a sparse matrix with long indices and float complex values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows      Number of rows
     * @param[in] cols      Number of columns
     * @param[in] nnz       Number of non-zero elements
     * @param[in] rowptr        Row pointer array (length = rows+1)
     * @param[in] colptr        Column index array (length = nnz)
     * @param[in] data      Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_write_float_complex_sparse_matrix_long function writes a sparse matrix
     * in CSR format using long for indices and float complex for values.
     */
    int csc_hdf5_write_float_complex_sparse_matrix_long(hid_t root, const char* dset_name, size_t rows, size_t cols, size_t nnz,
                                                        long* rowptr, long* colptr, float complex* data, csc_hdf5_options_t* opts);

    /**
     * @brief Write a sparse matrix with unsigned long indices and float complex values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[in] rows      Number of rows
     * @param[in] cols      Number of columns
     * @param[in] nnz       Number of non-zero elements
     * @param[in] rowptr        Row pointer array (length = rows+1)
     * @param[in] colptr        Column index array (length = nnz)
     * @param[in] data      Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_write_float_complex_sparse_matrix_ulong function writes a sparse matrix
     * in CSR format using unsigned long for indices and float complex for values.
     */
    int csc_hdf5_write_float_complex_sparse_matrix_ulong(hid_t root, const char* dset_name, size_t rows, size_t cols, size_t nnz,
                                                         unsigned long* rowptr, unsigned long* colptr, float complex* data,
                                                         csc_hdf5_options_t* opts);

    /**
     * @brief Read a sparse matrix with integer indices (int) and double values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] rows       Pointer to store the number of rows
     * @param[out] cols       Pointer to store the number of columns
     * @param[out] nnz        Pointer to store the number of non-zero elements
     * @param[out] rowptr     Row pointer array (length = rows+1)
     * @param[out] colptr     Column index array (length = nnz)
     * @param[out] values     Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_read_double_sparse_matrix_int function reads a sparse matrix
     * in CSR format using int for indices and double for values.
     */
    int csc_hdf5_read_double_sparse_matrix_int(hid_t root, const char* dset_name, size_t* rows, size_t* cols, size_t* nnz,
                                               int* rowptr, int* colptr, double* values, csc_hdf5_options_t* opts);

    /**
     * @brief Read a sparse matrix with unsigned int indices and double values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] rows       Pointer to store the number of rows
     * @param[out] cols       Pointer to store the number of columns
     * @param[out] nnz        Pointer to store the number of non-zero elements
     * @param[out] rowptr     Row pointer array (length = rows+1)
     * @param[out] colptr     Column index array (length = nnz)
     * @param[out] values     Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_read_double_sparse_matrix_uint function reads a sparse matrix
     * in CSR format using unsigned int for indices and double for values.
     */
    int csc_hdf5_read_double_sparse_matrix_uint(hid_t root, const char* dset_name, size_t* rows, size_t* cols, size_t* nnz,
                                                unsigned int* rowptr, unsigned int* colptr, double* values, csc_hdf5_options_t* opts);

    /**
     * @brief Read a sparse matrix with long indices and double values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] rows       Pointer to store the number of rows
     * @param[out] cols       Pointer to store the number of columns
     * @param[out] nnz        Pointer to store the number of non-zero elements
     * @param[out] rowptr     Row pointer array (length = rows+1)
     * @param[out] colptr     Column index array (length = nnz)
     * @param[out] values     Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_read_double_sparse_matrix_long function reads a sparse matrix
     * in CSR format using long for indices and double for values.
     */
    int csc_hdf5_read_double_sparse_matrix_long(hid_t root, const char* dset_name, size_t* rows, size_t* cols, size_t* nnz,
                                                long* rowptr, long* colptr, double* values, csc_hdf5_options_t* opts);

    /**
     * @brief Read a sparse matrix with unsigned long indices and double values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] rows       Pointer to store the number of rows
     * @param[out] cols       Pointer to store the number of columns
     * @param[out] nnz        Pointer to store the number of non-zero elements
     * @param[out] rowptr     Row pointer array (length = rows+1)
     * @param[out] colptr     Column index array (length = nnz)
     * @param[out] values     Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_read_double_sparse_matrix_ulong function reads a sparse matrix
     * in CSR format using unsigned long for indices and double for values.
     */
    int csc_hdf5_read_double_sparse_matrix_ulong(hid_t root, const char* dset_name, size_t* rows, size_t* cols, size_t* nnz,
                                                 unsigned long* rowptr, unsigned long* colptr, double* values,
                                                 csc_hdf5_options_t* opts);

    /**
     * @brief Read a sparse matrix with integer indices (int) and float values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] rows       Pointer to store the number of rows
     * @param[out] cols       Pointer to store the number of columns
     * @param[out] nnz        Pointer to store the number of non-zero elements
     * @param[out] rowptr     Row pointer array (length = rows+1)
     * @param[out] colptr     Column index array (length = nnz)
     * @param[out] values     Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_read_float_sparse_matrix_int function reads a sparse matrix
     * in CSR format using int for indices and float for values.
     */
    int csc_hdf5_read_float_sparse_matrix_int(hid_t root, const char* dset_name, size_t* rows, size_t* cols, size_t* nnz, int* rowptr,
                                              int* colptr, float* values, csc_hdf5_options_t* opts);

    /**
     * @brief Read a sparse matrix with unsigned int indices and float values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] rows       Pointer to store the number of rows
     * @param[out] cols       Pointer to store the number of columns
     * @param[out] nnz        Pointer to store the number of non-zero elements
     * @param[out] rowptr     Row pointer array (length = rows+1)
     * @param[out] colptr     Column index array (length = nnz)
     * @param[out] values     Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_read_float_sparse_matrix_uint function reads a sparse matrix
     * in CSR format using unsigned int for indices and float for values.
     */
    int csc_hdf5_read_float_sparse_matrix_uint(hid_t root, const char* dset_name, size_t* rows, size_t* cols, size_t* nnz,
                                               unsigned int* rowptr, unsigned int* colptr, float* values, csc_hdf5_options_t* opts);

    /**
     * @brief Read a sparse matrix with long indices and float values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] rows       Pointer to store the number of rows
     * @param[out] cols       Pointer to store the number of columns
     * @param[out] nnz        Pointer to store the number of non-zero elements
     * @param[out] rowptr     Row pointer array (length = rows+1)
     * @param[out] colptr     Column index array (length = nnz)
     * @param[out] values     Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_read_float_sparse_matrix_long function reads a sparse matrix
     * in CSR format using long for indices and float for values.
     */
    int csc_hdf5_read_float_sparse_matrix_long(hid_t root, const char* dset_name, size_t* rows, size_t* cols, size_t* nnz,
                                               long* rowptr, long* colptr, float* values, csc_hdf5_options_t* opts);

    /**
     * @brief Read a sparse matrix with unsigned long indices and float values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] rows       Pointer to store the number of rows
     * @param[out] cols       Pointer to store the number of columns
     * @param[out] nnz        Pointer to store the number of non-zero elements
     * @param[out] rowptr     Row pointer array (length = rows+1)
     * @param[out] colptr     Column index array (length = nnz)
     * @param[out] values     Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_read_float_sparse_matrix_ulong function reads a sparse matrix
     * in CSR format using unsigned long for indices and float for values.
     */
    int csc_hdf5_read_float_sparse_matrix_ulong(hid_t root, const char* dset_name, size_t* rows, size_t* cols, size_t* nnz,
                                                unsigned long* rowptr, unsigned long* colptr, float* values,
                                                csc_hdf5_options_t* opts);

    /**
     * @brief Read a sparse matrix with integer indices (int) and double complex values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] rows       Pointer to store the number of rows
     * @param[out] cols       Pointer to store the number of columns
     * @param[out] nnz        Pointer to store the number of non-zero elements
     * @param[out] rowptr     Row pointer array (length = rows+1)
     * @param[out] colptr     Column index array (length = nnz)
     * @param[out] values     Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_read_double_complex_sparse_matrix_int function reads a sparse matrix
     * in CSR format using int for indices and double complex for values.
     */
    int csc_hdf5_read_double_complex_sparse_matrix_int(hid_t root, const char* dset_name, size_t* rows, size_t* cols, size_t* nnz,
                                                       int* rowptr, int* colptr, double complex* values, csc_hdf5_options_t* opts);

    /**
     * @brief Read a sparse matrix with unsigned int indices and double complex values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] rows       Pointer to store the number of rows
     * @param[out] cols       Pointer to store the number of columns
     * @param[out] nnz        Pointer to store the number of non-zero elements
     * @param[out] rowptr     Row pointer array (length = rows+1)
     * @param[out] colptr     Column index array (length = nnz)
     * @param[out] values     Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_read_double_complex_sparse_matrix_uint function reads a sparse matrix
     * in CSR format using unsigned int for indices and double complex for values.
     */
    int csc_hdf5_read_double_complex_sparse_matrix_uint(hid_t root, const char* dset_name, size_t* rows, size_t* cols, size_t* nnz,
                                                        unsigned int* rowptr, unsigned int* colptr, double complex* values,
                                                        csc_hdf5_options_t* opts);

    /**
     * @brief Read a sparse matrix with long indices and double complex values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] rows       Pointer to store the number of rows
     * @param[out] cols       Pointer to store the number of columns
     * @param[out] nnz        Pointer to store the number of non-zero elements
     * @param[out] rowptr     Row pointer array (length = rows+1)
     * @param[out] colptr     Column index array (length = nnz)
     * @param[out] values     Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_read_double_complex_sparse_matrix_long function reads a sparse matrix
     * in CSR format using long for indices and double complex for values.
     */
    int csc_hdf5_read_double_complex_sparse_matrix_long(hid_t root, const char* dset_name, size_t* rows, size_t* cols, size_t* nnz,
                                                        long* rowptr, long* colptr, double complex* values, csc_hdf5_options_t* opts);

    /**
     * @brief Read a sparse matrix with unsigned long indices and double complex values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] rows       Pointer to store the number of rows
     * @param[out] cols       Pointer to store the number of columns
     * @param[out] nnz        Pointer to store the number of non-zero elements
     * @param[out] rowptr     Row pointer array (length = rows+1)
     * @param[out] colptr     Column index array (length = nnz)
     * @param[out] values     Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_read_double_complex_sparse_matrix_ulong function reads a sparse matrix
     * in CSR format using unsigned long for indices and double complex for values.
     */
    int csc_hdf5_read_double_complex_sparse_matrix_ulong(hid_t root, const char* dset_name, size_t* rows, size_t* cols, size_t* nnz,
                                                         unsigned long* rowptr, unsigned long* colptr, double complex* values,
                                                         csc_hdf5_options_t* opts);

    /**
     * @brief Read a sparse matrix with integer indices (int) and float complex values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] rows       Pointer to store the number of rows
     * @param[out] cols       Pointer to store the number of columns
     * @param[out] nnz        Pointer to store the number of non-zero elements
     * @param[out] rowptr     Row pointer array (length = rows+1)
     * @param[out] colptr     Column index array (length = nnz)
     * @param[out] values     Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_read_float_complex_sparse_matrix_int function reads a sparse matrix
     * in CSR format using int for indices and float complex for values.
     */
    int csc_hdf5_read_float_complex_sparse_matrix_int(hid_t root, const char* dset_name, size_t* rows, size_t* cols, size_t* nnz,
                                                      int* rowptr, int* colptr, float complex* values, csc_hdf5_options_t* opts);

    /**
     * @brief Read a sparse matrix with unsigned int indices and float complex values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] rows       Pointer to store the number of rows
     * @param[out] cols       Pointer to store the number of columns
     * @param[out] nnz        Pointer to store the number of non-zero elements
     * @param[out] rowptr     Row pointer array (length = rows+1)
     * @param[out] colptr     Column index array (length = nnz)
     * @param[out] values     Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_read_float_complex_sparse_matrix_uint function reads a sparse matrix
     * in CSR format using unsigned int for indices and float complex for values.
     */
    int csc_hdf5_read_float_complex_sparse_matrix_uint(hid_t root, const char* dset_name, size_t* rows, size_t* cols, size_t* nnz,
                                                       unsigned int* rowptr, unsigned int* colptr, float complex* values,
                                                       csc_hdf5_options_t* opts);

    /**
     * @brief Read a sparse matrix with long indices and float complex values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] rows       Pointer to store the number of rows
     * @param[out] cols       Pointer to store the number of columns
     * @param[out] nnz        Pointer to store the number of non-zero elements
     * @param[out] rowptr     Row pointer array (length = rows+1)
     * @param[out] colptr     Column index array (length = nnz)
     * @param[out] values     Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_read_float_complex_sparse_matrix_long function reads a sparse matrix
     * in CSR format using long for indices and float complex for values.
     */
    int csc_hdf5_read_float_complex_sparse_matrix_long(hid_t root, const char* dset_name, size_t* rows, size_t* cols, size_t* nnz,
                                                       long* rowptr, long* colptr, float complex* values, csc_hdf5_options_t* opts);

    /**
     * @brief Read a sparse matrix with unsigned long indices and float complex values.
     * @param[in] root      HDF5 file or group identifier
     * @param[in] dset_name     Name of the dataset
     * @param[out] rows       Pointer to store the number of rows
     * @param[out] cols       Pointer to store the number of columns
     * @param[out] nnz        Pointer to store the number of non-zero elements
     * @param[out] rowptr     Row pointer array (length = rows+1)
     * @param[out] colptr     Column index array (length = nnz)
     * @param[out] values     Array of non-zero values (length = nnz)
     * @param[in] opts          Pointer to HDF5 options (can be NULL for defaults)
     * @return int              0 on success, negative error code otherwise
     *
     * The csc_hdf5_read_float_complex_sparse_matrix_ulong function reads a sparse matrix
     * in CSR format using unsigned long for indices and float complex for values.
     */
    int csc_hdf5_read_float_complex_sparse_matrix_ulong(hid_t root, const char* dset_name, size_t* rows, size_t* cols, size_t* nnz,
                                                        unsigned long* rowptr, unsigned long* colptr, float complex* values,
                                                        csc_hdf5_options_t* opts);

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
    int csc_hdf5_matrix_size(hid_t root, const char* dset_name, size_t* rows, size_t* cols);

    /**
     * @brief Return the size of a sparse matrix inside an HDF5 storage.
     * @param[in]   root        root id of the dataset
     * @param[in]   dset_name   name of the dataset.
     * @param[out]  rows        The number of rows of the matrix
     * @param[out]  cols        The number of columns of the matrix
     * @param[out]  nnz         The number of non-zero elements
     * @return zero on success or a non-zero error code.
     *
     * The csc_hdf5_sparse_size function reads the size of a sparse matrix from a
     * given data set addressed by root and dset_name.
     */
    int csc_hdf5_sparse_size(hid_t root, const char* dset_name, size_t* rows, size_t* cols, size_t* nnz);

    /**
     * @}
     */

#undef DEPRECATE
#undef DEPRECATE_MSG
#ifdef __cplusplus
};
#endif

#endif /* end of include guard: CSC_HDF_H */
