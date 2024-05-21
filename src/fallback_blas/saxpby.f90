! SPDX-License-Identifier: LGPL-3.0-or-later
!
! This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
! Copyright (C) 2013-2024 Martin Koehler
!
! This program is free software: you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by the Free
! Software Foundation, either version 3 of the License, or (at your option)
! any later version.
!
! This program is distributed in the hope that it will be useful, but WITHOUT
! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
! FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
! more details.
!
! You should have received a copy of the GNU General Public License along
! with this program. If not, see <https://www.gnu.org/licenses/>.


SUBROUTINE SAXPBY(N,SA,SX,INCX,SB,SY,INCY)
    !     .. Scalar Arguments ..
    REAL SA,SB
    INTEGER INCX,INCY,N
    !     ..
    !     .. Array Arguments ..
    REAL SX(*),SY(*)
    !     ..
    !
    !  Purpose
    !  =======
    !
    !     SAXPY constant times a vector plus constant times a vector.
    !     uses unrolled loops for increments equal to one.
    !
    !  Further Details
    !  ===============
    !
    !     jack dongarra, linpack, 3/11/78.
    !     Martin Koehler, FlexiBLAS 9/5/2014
    !     modified 12/3/93, array(1) declarations changed to array(*)
    !     modified 09/05/2014, axpy -> axpby
    !
    !  =====================================================================
    !
    !     .. Local Scalars ..
    INTEGER I,IX,IY,M,MP1
    !     ..
    !     .. Intrinsic Functions ..
    INTRINSIC MOD
    !     ..
    IF (N.LE.0) RETURN
    IF (SA.EQ.0.0 .AND. SB.EQ.0.0) RETURN
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
                SY(I) = SB*SY(I) + SA*SX(I)
            END DO
        END IF
        IF (N.LT.4) RETURN
        MP1 = M + 1
        DO I = MP1,N,4
            SY(I) =   SB*SY(I) + SA*SX(I)
            SY(I+1) = SB*SY(I+1) + SA*SX(I+1)
            SY(I+2) = SB*SY(I+2) + SA*SX(I+2)
            SY(I+3) = SB*SY(I+3) + SA*SX(I+3)
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
            SY(IY) = SB*SY(IY) + SA*SX(IX)
            IX = IX + INCX
            IY = IY + INCY
        END DO
    END IF
    RETURN
END SUBROUTINE
