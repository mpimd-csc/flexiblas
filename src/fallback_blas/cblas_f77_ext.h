/*
 * cblas_f77.h
 * Written by Keita Teranishi
 *
 * Updated by Jeff Horner
 * Merged cblas_f77.h and cblas_fortran_header.h
 */

#ifndef CBLAS_F77_EXT_H
#define CBLAS_F77_EXT_H

#include "fc.h"

#include <stdarg.h>
#include <stdint.h>
/*
 * Level 1 BLAS
 */

#define F77_saxpby_base        F77_GLOBAL_SUFFIX(saxpby,SAXPBY)
#define F77_daxpby_base        F77_GLOBAL_SUFFIX(daxpby,DAXPBY)
#define F77_caxpby_base        F77_GLOBAL_SUFFIX(caxpby,CAXPBY)
#define F77_zaxpby_base        F77_GLOBAL_SUFFIX(zaxpby,ZAXPBY)

/*
 * Level 2 BLAS
 */

/*
 * Level 3 BLAS
 */

#define F77_sgeadd_base        F77_GLOBAL_SUFFIX(sgeadd,SGEADD)
#define F77_dgeadd_base        F77_GLOBAL_SUFFIX(dgeadd,DGEADD)
#define F77_cgeadd_base        F77_GLOBAL_SUFFIX(cgeadd,CGEADD)
#define F77_zgeadd_base        F77_GLOBAL_SUFFIX(zgeadd,ZGEADD)

#define F77_somatcopy_base        F77_GLOBAL_SUFFIX(somatcopy,SOMATCOPY)
#define F77_domatcopy_base        F77_GLOBAL_SUFFIX(domatcopy,DOMATCOPY)
#define F77_comatcopy_base        F77_GLOBAL_SUFFIX(comatcopy,COMATCOPY)
#define F77_zomatcopy_base        F77_GLOBAL_SUFFIX(zomatcopy,ZOMATCOPY)

#define F77_simatcopy_base        F77_GLOBAL_SUFFIX(simatcopy,SIMATCOPY)
#define F77_dimatcopy_base        F77_GLOBAL_SUFFIX(dimatcopy,DIMATCOPY)
#define F77_cimatcopy_base        F77_GLOBAL_SUFFIX(cimatcopy,CIMATCOPY)
#define F77_zimatcopy_base        F77_GLOBAL_SUFFIX(zimatcopy,ZIMATCOPY)



/*
 * Level 1 Fortran variadic definitions
 */

#define F77_saxpby(...)        F77_saxpby_base(__VA_ARGS__)
#define F77_daxpby(...)        F77_daxpby_base(__VA_ARGS__)
#define F77_caxpby(...)        F77_caxpby_base(__VA_ARGS__)
#define F77_zaxpby(...)        F77_zaxpby_base(__VA_ARGS__)

/*
 * Level 2 Fortran variadic definitions without FCHAR
 */

#ifdef BLAS_FORTRAN_STRLEN_END

/*
 * Level 2 Fortran variadic definitions with BLAS_FORTRAN_STRLEN_END
 */

/*
 * Level 3 Fortran variadic definitions with BLAS_FORTRAN_STRLEN_END
 */
#define F77_sgeadd(...)        F77_sgeadd_base(__VA_ARGS__)
#define F77_dgeadd(...)        F77_dgeadd_base(__VA_ARGS__)
#define F77_cgeadd(...)        F77_cgeadd_base(__VA_ARGS__)
#define F77_zgeadd(...)        F77_zgeadd_base(__VA_ARGS__)

#define F77_somatcopy(...)        F77_somatcopy_base(__VA_ARGS__,1,1)
#define F77_domatcopy(...)        F77_domatcopy_base(__VA_ARGS__,1,1)
#define F77_comatcopy(...)        F77_comatcopy_base(__VA_ARGS__,1,1)
#define F77_zomatcopy(...)        F77_zomatcopy_base(__VA_ARGS__,1,1)

#define F77_simatcopy(...)        F77_simatcopy_base(__VA_ARGS__,1,1)
#define F77_dimatcopy(...)        F77_dimatcopy_base(__VA_ARGS__,1,1)
#define F77_cimatcopy(...)        F77_cimatcopy_base(__VA_ARGS__,1,1)
#define F77_zimatcopy(...)        F77_zimatcopy_base(__VA_ARGS__,1,1)




#else

/*
 * Level 2 Fortran variadic definitions without BLAS_FORTRAN_STRLEN_END
 */

/*
 * Level 3 Fortran variadic definitions without BLAS_FORTRAN_STRLEN_END
 */
#define F77_sgeadd(...)        F77_sgeadd_base(__VA_ARGS__)
#define F77_dgeadd(...)        F77_dgeadd_base(__VA_ARGS__)
#define F77_cgeadd(...)        F77_cgeadd_base(__VA_ARGS__)
#define F77_zgeadd(...)        F77_zgeadd_base(__VA_ARGS__)

#define F77_somatcopy(...)        F77_somatcopy_base(__VA_ARGS__)
#define F77_domatcopy(...)        F77_domatcopy_base(__VA_ARGS__)
#define F77_comatcopy(...)        F77_comatcopy_base(__VA_ARGS__)
#define F77_zomatcopy(...)        F77_zomatcopy_base(__VA_ARGS__)

#define F77_simatcopy(...)        F77_simatcopy_base(__VA_ARGS__)
#define F77_dimatcopy(...)        F77_dimatcopy_base(__VA_ARGS__)
#define F77_cimatcopy(...)        F77_cimatcopy_base(__VA_ARGS__)
#define F77_zimatcopy(...)        F77_zimatcopy_base(__VA_ARGS__)



#endif

/*
 * Base function prototypes
 */

#ifdef __cplusplus
extern "C" {
#endif

    /*
     * Level 1 Fortran Prototypes
     */

    void F77_saxpby_base(FINT, const float *, const float *, FINT, const float*, float *, FINT);
    void F77_daxpby_base(FINT, const double *, const double *, FINT, const double *, double *, FINT);
    void F77_caxpby_base(FINT, const void *, const void *, FINT, const void * , void *, FINT);
    void F77_zaxpby_base(FINT, const void *, const void *, FINT, const void * , void *, FINT);

    /*
     * Level 2 Fortran Prototypes
     */

    /*
     * Level 3 Fortran Prototypes
     */
    void F77_sgeadd_base(FINT, FINT, const float *, const float *, FINT, const float *, float *, FINT );
    void F77_dgeadd_base(FINT, FINT, const double *, const double *, FINT, const double *, double *, FINT );
    void F77_cgeadd_base(FINT, FINT, const void *, const void *, FINT, const void *, void *, FINT );
    void F77_zgeadd_base(FINT, FINT, const void *, const void *, FINT, const void *, void *, FINT );

    void F77_somatcopy_base(const char*, const char*, FINT, FINT, const float*, const float*, FINT, float*, FINT
#ifdef BLAS_FORTRAN_STRLEN_END
            , FORTRAN_STRLEN, FORTRAN_STRLEN
#endif
            );
    void F77_domatcopy_base(const char*, const char*, FINT, FINT, const double*, const double*, FINT, double*, FINT
#ifdef BLAS_FORTRAN_STRLEN_END
            , FORTRAN_STRLEN, FORTRAN_STRLEN
#endif
            );

    void F77_comatcopy_base(const char*, const char*, FINT, FINT, const void*, const void*, FINT, void*, FINT
#ifdef BLAS_FORTRAN_STRLEN_END
            , FORTRAN_STRLEN, FORTRAN_STRLEN
#endif
            );

    void F77_zomatcopy_base(const char*, const char*, FINT, FINT, const void*, const void*, FINT, void*, FINT
#ifdef BLAS_FORTRAN_STRLEN_END
            , FORTRAN_STRLEN, FORTRAN_STRLEN
#endif
            );

    void F77_simatcopy_base(const char*, const char*, FINT, FINT, const float*, float*, FINT, FINT
#ifdef BLAS_FORTRAN_STRLEN_END
            , FORTRAN_STRLEN, FORTRAN_STRLEN
#endif
            );
    void F77_dimatcopy_base(const char*, const char*, FINT, FINT, const double*, double*, FINT, FINT
#ifdef BLAS_FORTRAN_STRLEN_END
            , FORTRAN_STRLEN, FORTRAN_STRLEN
#endif
            );

    void F77_cimatcopy_base(const char*, const char*, FINT, FINT, const void*, void*, FINT, FINT
#ifdef BLAS_FORTRAN_STRLEN_END
            , FORTRAN_STRLEN, FORTRAN_STRLEN
#endif
            );

    void F77_zimatcopy_base(const char*, const char*, FINT, FINT, const void*, void*, FINT, FINT
#ifdef BLAS_FORTRAN_STRLEN_END
            , FORTRAN_STRLEN, FORTRAN_STRLEN
#endif
            );

#ifdef __cplusplus
}
#endif

#endif /*  CBLAS_F77_H */
