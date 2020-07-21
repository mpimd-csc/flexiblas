!
! This program is free software; you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! Linking FlexiBLAS statically or dynamically with other modules is making a
! combined work based on FlexiBLAS. Thus, the terms and conditions of the GNU
! General Public License cover the whole combination.
!
! As a special exception, the copyright holders of FlexiBLAS give you permission
! to combine FlexiBLAS program with free software programs or libraries that are
! released under the GNU LGPL and with independent modules that communicate with
! FlexiBLAS solely through the BLAS/LAPACK interface as provided by the
! BLAS/LAPACK reference implementation. You may copy and distribute such a system
! following the terms of the GNU GPL for FlexiBLAS and the licenses of the other
! code concerned, provided that you include the source code of that other code
! when and as the GNU GPL requires distribution of source code and provided that
! you do not modify the BLAS/LAPACK interface.
!
! Note that people who make modified versions of FlexiBLAS are not obligated to
! grant this special exception for their modified versions; it is their choice
! whether to do so. The GNU General Public License gives permission to release a
! modified version without this exception; this exception also makes it possible
! to release a modified version which carries forward this exception. If you
! modify the BLAS/LAPACK interface, this exception does not apply to your
! modified version of FlexiBLAS, and you must remove this exception when you
! distribute your modified version.
!
! This exception is an additional permission under section 7 of the GNU General
! Public License, version 3 (“GPLv3”)
!
!
! You should have received a copy of the GNU General Public License
! along with this program; if not, see <http://www.gnu.org/licenses/>.
!
! Copyright (C) Martin Koehler, 2013-2020
!



SUBROUTINE DAXPBY(N,DA,DX,INCX,DB,DY,INCY)
    !     .. Scalar Arguments ..
    DOUBLE PRECISION DA, DB
    INTEGER*4 INCX,INCY,N
    !     ..
    !     .. Array Arguments ..
    DOUBLE PRECISION DX(*),DY(*)
    !     ..
    !
    !  Purpose
    !  =======
    !
    !     DAXPY constant times a vector plus constant times a  vector.
    !     uses unrolled loops for increments equal to one.
    !
    !  Further Details
    !  ===============
    !
    !     jack dongarra, linpack, 3/11/78.
    !     Martin Koehler, FlexiBLAS 9/5/2014
    !     modified 12/3/93, array(1) declarations changed to array(*)
    !     modified 09/05/2014, axpy -> axpby
    !  =====================================================================
    !
    !     .. Local Scalars ..
    INTEGER*4 I,IX,IY,M,MP1
    !     ..
    !     .. Intrinsic Functions ..
    INTRINSIC MOD
    !     ..
    IF (N.LE.0) RETURN
    IF (DA.EQ.0.0d0 .AND. DB.EQ.0.0d0) RETURN
    IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
        !
        !        code for both increments equal to 1
        !
        !
        !        clean-up loop
        !
        M = MOD(N,4)
        IF (M.NE.0) THEN
            DO I = 1,M
                DY(I) = DB*DY(I) + DA*DX(I)
            END DO
        END IF
        IF (N.LT.4) RETURN
        MP1 = M + 1
        DO I = MP1,N,4
            DY(I)   = DB*DY(I) + DA*DX(I)
            DY(I+1) = DB*DY(I+1) + DA*DX(I+1)
            DY(I+2) = DB*DY(I+2) + DA*DX(I+2)
            DY(I+3) = DB*DY(I+3) + DA*DX(I+3)
        END DO
    ELSE
        !
        !        code for unequal increments or equal increments
        !          not equal to 1
        !
        IX = 1
        IY = 1
        IF (INCX.LT.0) IX = (-N+1)*INCX + 1
        IF (INCY.LT.0) IY = (-N+1)*INCY + 1
        DO I = 1,N
            DY(IY) = DB*DY(IY) + DA*DX(IX)
            IX = IX + INCX
            IY = IY + INCY
        END DO
    END IF
    RETURN
END SUBROUTINE


