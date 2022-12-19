/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Linking FlexiBLAS statically or dynamically with other modules is making a
 * combined work based on FlexiBLAS. Thus, the terms and conditions of the GNU
 * General Public License cover the whole combination.
 *
 * As a special exception, the copyright holders of FlexiBLAS give you permission
 * to combine FlexiBLAS program with free software programs or libraries that are
 * released under the GNU LGPL and with independent modules that communicate with
 * FlexiBLAS solely through the BLAS/LAPACK interface as provided by the
 * BLAS/LAPACK reference implementation. You may copy and distribute such a system
 * following the terms of the GNU GPL for FlexiBLAS and the licenses of the other
 * code concerned, provided that you include the source code of that other code
 * when and as the GNU GPL requires distribution of source code and provided that
 * you do not modify the BLAS/LAPACK interface.
 *
 * Note that people who make modified versions of FlexiBLAS are not obligated to
 * grant this special exception for their modified versions; it is their choice
 * whether to do so. The GNU General Public License gives permission to release a
 * modified version without this exception; this exception also makes it possible
 * to release a modified version which carries forward this exception. If you
 * modify the BLAS/LAPACK interface, this exception does not apply to your
 * modified version of FlexiBLAS, and you must remove this exception when you
 * distribute your modified version.
 *
 * This exception is an additional permission under section 7 of the GNU General
 * Public License, version 3 (“GPLv3”)
 *
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2013-2022
 */



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "cblas.h"
#include "cblas_f77.h"
#include "../flexiblas.h"

#ifdef __ELF__
void internal_cblas_xerbla(CBLAS_INT info, const char *rout, const char *form, ...);
void cblas_xerbla(CBLAS_INT info, const char *, const char *, ...) __attribute__ ((weak, alias ("internal_cblas_xerbla")));
void internal_cblas_xerbla(CBLAS_INT info, const char *rout, const char *form, ...)
#else
void internal_cblas_xerbla(CBLAS_INT info, const char *rout, const char *form, ...)
#endif
{
   extern int RowMajorStrg;
   char empty[1] = "";
   va_list argptr;

   va_start(argptr, form);


   if (RowMajorStrg)
   {
      if (strstr(rout,"gemm") != 0)
      {
         if      (info == 5 ) info =  4;
         else if (info == 4 ) info =  5;
         else if (info == 11) info =  9;
         else if (info == 9 ) info = 11;
      }
      else if (strstr(rout,"symm") != 0 || strstr(rout,"hemm") != 0)
      {
         if      (info == 5 ) info =  4;
         else if (info == 4 ) info =  5;
      }
      else if (strstr(rout,"trmm") != 0 || strstr(rout,"trsm") != 0)
      {
         if      (info == 7 ) info =  6;
         else if (info == 6 ) info =  7;
      }
      else if (strstr(rout,"gemv") != 0)
      {
         if      (info == 4)  info = 3;
         else if (info == 3)  info = 4;
      }
      else if (strstr(rout,"gbmv") != 0)
      {
         if      (info == 4)  info = 3;
         else if (info == 3)  info = 4;
         else if (info == 6)  info = 5;
         else if (info == 5)  info = 6;
      }
      else if (strstr(rout,"ger") != 0)
      {
         if      (info == 3) info = 2;
         else if (info == 2) info = 3;
         else if (info == 8) info = 6;
         else if (info == 6) info = 8;
      }
      else if ( (strstr(rout,"her2") != 0 || strstr(rout,"hpr2") != 0)
                 && strstr(rout,"her2k") == 0 )
      {
         if      (info == 8) info = 6;
         else if (info == 6) info = 8;
      }
   }

   if (info)
      fprintf(stderr, "Parameter %" CBLAS_IFMT "  to routine %s was incorrect\n", info, rout);
   vfprintf(stderr, form, argptr);
   va_end(argptr);
   if (info) {
       if ( !info)
           xerbla_(empty, &info);
   }
}


#ifdef __ELF__
CBLAS_INT  internal_cblas_errprn(CBLAS_INT ierr, CBLAS_INT info, const char *form, ...);
CBLAS_INT  cblas_errprn(CBLAS_INT ierr, CBLAS_INT info, const char *, ...) __attribute__ ((weak, alias ("internal_cblas_errprn")));
CBLAS_INT  internal_cblas_errprn(CBLAS_INT ierr, CBLAS_INT info, const char *form, ...)
#else
CBLAS_INT cblas_errprn(CBLAS_INT ierr, CBLAS_INT info,const char *form, ...)
#endif
{

   va_list argptr;

   va_start(argptr, form);
#ifdef GCCWIN
   vprintf(form, argptr);
#else
   vfprintf(stderr, form, argptr);
#endif
   va_end(argptr);
   if ( ierr < info )
	   return ierr;
   else
	   return info;
}
