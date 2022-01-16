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
! Copyright (C) Martin Koehler, 2013-2021
!




SUBROUTINE CGEADD(M, N, ALPHA, A, LDA, BETA, B, LDB)
    ! B:= alpha * A + beta *B
    IMPLICIT NONE
    INTEGER*4 M, N, LDA, LDB
    COMPLEX ALPHA, BETA
    COMPLEX A(LDA,*), B(LDB, *)

    ! Locals
    INTEGER*4 COL
    EXTERNAL CAXPBY, XERBLA
    INTRINSIC MAX

    IF (M .LE. 0 ) RETURN
    IF (N .LE. 0 ) RETURN
    IF (MAX(1,LDA) .LT. M) THEN
        CALL XERBLA("CGEADD", 5)
    ENDIF
    IF (MAX(1,LDB) .LT. M) THEN
        CALL XERBLA("CGEADD", 8)
    ENDIF

    DO COL = 1, N
        CALL CAXPBY(M, ALPHA, A(1,COL), LDA,  BETA, B(1,COL), LDB)
    END DO
END SUBROUTINE
