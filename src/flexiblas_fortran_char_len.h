#ifndef FLEIXBLAS_FORTRAN_CHAR_LEN_H_
#define FLEIXBLAS_FORTRAN_CHAR_LEN_H_
#include <stdint.h>

#ifndef FLEXIBLAS_CHARLEN_T
#define FLEXIBLAS_CHARLEN_T

#if defined(__INTEL_LLVM_COMPILER) || defined(__ICC)
/* Intel Compiler (oneAPI and classic) */
typedef size_t flexiblas_fortran_charlen_t;
#elif defined(__aocc__)
/* CLANG/FLANG as AMD AOCC */
typedef size_t flexiblas_fortran_charlen_t;
#elif defined(__clang__)
/* CLANG/FLANG */
typedef size_t flexiblas_fortran_charlen_t;
#elif __GNUC__ > 7
/* GNU 8.x and newer */
typedef size_t flexiblas_fortran_charlen_t;
#else
/* GNU 4.x - 7.x */
typedef int flexiblas_fortran_charlen_t;
#endif
#endif

#ifndef blasint
#ifdef FLEXIBLAS_INTEGER8
#include <stdint.h>
#define blasint int64_t
#else
#define blasint int
#endif
#endif


#endif
