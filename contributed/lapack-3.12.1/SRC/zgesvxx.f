*> \brief <b> ZGESVXX computes the solution to system of linear equations A * X = B for GE matrices</b>
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download ZGESVXX + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/zgesvxx.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/zgesvxx.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/zgesvxx.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE ZGESVXX( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF, IPIV,
*                           EQUED, R, C, B, LDB, X, LDX, RCOND, RPVGRW,
*                           BERR, N_ERR_BNDS, ERR_BNDS_NORM,
*                           ERR_BNDS_COMP, NPARAMS, PARAMS, WORK, RWORK,
*                           INFO )
*
*       .. Scalar Arguments ..
*       CHARACTER          EQUED, FACT, TRANS
*       INTEGER            INFO, LDA, LDAF, LDB, LDX, N, NRHS, NPARAMS,
*      $                   N_ERR_BNDS
*       DOUBLE PRECISION   RCOND, RPVGRW
*       ..
*       .. Array Arguments ..
*       INTEGER            IPIV( * )
*       COMPLEX*16         A( LDA, * ), AF( LDAF, * ), B( LDB, * ),
*      $                   X( LDX , * ),WORK( * )
*       DOUBLE PRECISION   R( * ), C( * ), PARAMS( * ), BERR( * ),
*      $                   ERR_BNDS_NORM( NRHS, * ),
*      $                   ERR_BNDS_COMP( NRHS, * ), RWORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*>    ZGESVXX uses the LU factorization to compute the solution to a
*>    complex*16 system of linear equations  A * X = B,  where A is an
*>    N-by-N matrix and X and B are N-by-NRHS matrices.
*>
*>    If requested, both normwise and maximum componentwise error bounds
*>    are returned. ZGESVXX will return a solution with a tiny
*>    guaranteed error (O(eps) where eps is the working machine
*>    precision) unless the matrix is very ill-conditioned, in which
*>    case a warning is returned. Relevant condition numbers also are
*>    calculated and returned.
*>
*>    ZGESVXX accepts user-provided factorizations and equilibration
*>    factors; see the definitions of the FACT and EQUED options.
*>    Solving with refinement and using a factorization from a previous
*>    ZGESVXX call will also produce a solution with either O(eps)
*>    errors or warnings, but we cannot make that claim for general
*>    user-provided factorizations and equilibration factors if they
*>    differ from what ZGESVXX would itself produce.
*> \endverbatim
*
*> \par Description:
*  =================
*>
*> \verbatim
*>
*>    The following steps are performed:
*>
*>    1. If FACT = 'E', double precision scaling factors are computed to equilibrate
*>    the system:
*>
*>      TRANS = 'N':  diag(R)*A*diag(C)     *inv(diag(C))*X = diag(R)*B
*>      TRANS = 'T': (diag(R)*A*diag(C))**T *inv(diag(R))*X = diag(C)*B
*>      TRANS = 'C': (diag(R)*A*diag(C))**H *inv(diag(R))*X = diag(C)*B
*>
*>    Whether or not the system will be equilibrated depends on the
*>    scaling of the matrix A, but if equilibration is used, A is
*>    overwritten by diag(R)*A*diag(C) and B by diag(R)*B (if TRANS='N')
*>    or diag(C)*B (if TRANS = 'T' or 'C').
*>
*>    2. If FACT = 'N' or 'E', the LU decomposition is used to factor
*>    the matrix A (after equilibration if FACT = 'E') as
*>
*>      A = P * L * U,
*>
*>    where P is a permutation matrix, L is a unit lower triangular
*>    matrix, and U is upper triangular.
*>
*>    3. If some U(i,i)=0, so that U is exactly singular, then the
*>    routine returns with INFO = i. Otherwise, the factored form of A
*>    is used to estimate the condition number of the matrix A (see
*>    argument RCOND). If the reciprocal of the condition number is less
*>    than machine precision, the routine still goes on to solve for X
*>    and compute error bounds as described below.
*>
*>    4. The system of equations is solved for X using the factored form
*>    of A.
*>
*>    5. By default (unless PARAMS(LA_LINRX_ITREF_I) is set to zero),
*>    the routine will use iterative refinement to try to get a small
*>    error and error bounds.  Refinement calculates the residual to at
*>    least twice the working precision.
*>
*>    6. If equilibration was used, the matrix X is premultiplied by
*>    diag(C) (if TRANS = 'N') or diag(R) (if TRANS = 'T' or 'C') so
*>    that it solves the original system before equilibration.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \verbatim
*>     Some optional parameters are bundled in the PARAMS array.  These
*>     settings determine how refinement is performed, but often the
*>     defaults are acceptable.  If the defaults are acceptable, users
*>     can pass NPARAMS = 0 which prevents the source code from accessing
*>     the PARAMS argument.
*> \endverbatim
*>
*> \param[in] FACT
*> \verbatim
*>          FACT is CHARACTER*1
*>     Specifies whether or not the factored form of the matrix A is
*>     supplied on entry, and if not, whether the matrix A should be
*>     equilibrated before it is factored.
*>       = 'F':  On entry, AF and IPIV contain the factored form of A.
*>               If EQUED is not 'N', the matrix A has been
*>               equilibrated with scaling factors given by R and C.
*>               A, AF, and IPIV are not modified.
*>       = 'N':  The matrix A will be copied to AF and factored.
*>       = 'E':  The matrix A will be equilibrated if necessary, then
*>               copied to AF and factored.
*> \endverbatim
*>
*> \param[in] TRANS
*> \verbatim
*>          TRANS is CHARACTER*1
*>     Specifies the form of the system of equations:
*>       = 'N':  A * X = B     (No transpose)
*>       = 'T':  A**T * X = B  (Transpose)
*>       = 'C':  A**H * X = B  (Conjugate Transpose)
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>     The number of linear equations, i.e., the order of the
*>     matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in] NRHS
*> \verbatim
*>          NRHS is INTEGER
*>     The number of right hand sides, i.e., the number of columns
*>     of the matrices B and X.  NRHS >= 0.
*> \endverbatim
*>
*> \param[in,out] A
*> \verbatim
*>          A is COMPLEX*16 array, dimension (LDA,N)
*>     On entry, the N-by-N matrix A.  If FACT = 'F' and EQUED is
*>     not 'N', then A must have been equilibrated by the scaling
*>     factors in R and/or C.  A is not modified if FACT = 'F' or
*>     'N', or if FACT = 'E' and EQUED = 'N' on exit.
*>
*>     On exit, if EQUED .ne. 'N', A is scaled as follows:
*>     EQUED = 'R':  A := diag(R) * A
*>     EQUED = 'C':  A := A * diag(C)
*>     EQUED = 'B':  A := diag(R) * A * diag(C).
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>     The leading dimension of the array A.  LDA >= max(1,N).
*> \endverbatim
*>
*> \param[in,out] AF
*> \verbatim
*>          AF is COMPLEX*16 array, dimension (LDAF,N)
*>     If FACT = 'F', then AF is an input argument and on entry
*>     contains the factors L and U from the factorization
*>     A = P*L*U as computed by ZGETRF.  If EQUED .ne. 'N', then
*>     AF is the factored form of the equilibrated matrix A.
*>
*>     If FACT = 'N', then AF is an output argument and on exit
*>     returns the factors L and U from the factorization A = P*L*U
*>     of the original matrix A.
*>
*>     If FACT = 'E', then AF is an output argument and on exit
*>     returns the factors L and U from the factorization A = P*L*U
*>     of the equilibrated matrix A (see the description of A for
*>     the form of the equilibrated matrix).
*> \endverbatim
*>
*> \param[in] LDAF
*> \verbatim
*>          LDAF is INTEGER
*>     The leading dimension of the array AF.  LDAF >= max(1,N).
*> \endverbatim
*>
*> \param[in,out] IPIV
*> \verbatim
*>          IPIV is INTEGER array, dimension (N)
*>     If FACT = 'F', then IPIV is an input argument and on entry
*>     contains the pivot indices from the factorization A = P*L*U
*>     as computed by ZGETRF; row i of the matrix was interchanged
*>     with row IPIV(i).
*>
*>     If FACT = 'N', then IPIV is an output argument and on exit
*>     contains the pivot indices from the factorization A = P*L*U
*>     of the original matrix A.
*>
*>     If FACT = 'E', then IPIV is an output argument and on exit
*>     contains the pivot indices from the factorization A = P*L*U
*>     of the equilibrated matrix A.
*> \endverbatim
*>
*> \param[in,out] EQUED
*> \verbatim
*>          EQUED is CHARACTER*1
*>     Specifies the form of equilibration that was done.
*>       = 'N':  No equilibration (always true if FACT = 'N').
*>       = 'R':  Row equilibration, i.e., A has been premultiplied by
*>               diag(R).
*>       = 'C':  Column equilibration, i.e., A has been postmultiplied
*>               by diag(C).
*>       = 'B':  Both row and column equilibration, i.e., A has been
*>               replaced by diag(R) * A * diag(C).
*>     EQUED is an input argument if FACT = 'F'; otherwise, it is an
*>     output argument.
*> \endverbatim
*>
*> \param[in,out] R
*> \verbatim
*>          R is DOUBLE PRECISION array, dimension (N)
*>     The row scale factors for A.  If EQUED = 'R' or 'B', A is
*>     multiplied on the left by diag(R); if EQUED = 'N' or 'C', R
*>     is not accessed.  R is an input argument if FACT = 'F';
*>     otherwise, R is an output argument.  If FACT = 'F' and
*>     EQUED = 'R' or 'B', each element of R must be positive.
*>     If R is output, each element of R is a power of the radix.
*>     If R is input, each element of R should be a power of the radix
*>     to ensure a reliable solution and error estimates. Scaling by
*>     powers of the radix does not cause rounding errors unless the
*>     result underflows or overflows. Rounding errors during scaling
*>     lead to refining with a matrix that is not equivalent to the
*>     input matrix, producing error estimates that may not be
*>     reliable.
*> \endverbatim
*>
*> \param[in,out] C
*> \verbatim
*>          C is DOUBLE PRECISION array, dimension (N)
*>     The column scale factors for A.  If EQUED = 'C' or 'B', A is
*>     multiplied on the right by diag(C); if EQUED = 'N' or 'R', C
*>     is not accessed.  C is an input argument if FACT = 'F';
*>     otherwise, C is an output argument.  If FACT = 'F' and
*>     EQUED = 'C' or 'B', each element of C must be positive.
*>     If C is output, each element of C is a power of the radix.
*>     If C is input, each element of C should be a power of the radix
*>     to ensure a reliable solution and error estimates. Scaling by
*>     powers of the radix does not cause rounding errors unless the
*>     result underflows or overflows. Rounding errors during scaling
*>     lead to refining with a matrix that is not equivalent to the
*>     input matrix, producing error estimates that may not be
*>     reliable.
*> \endverbatim
*>
*> \param[in,out] B
*> \verbatim
*>          B is COMPLEX*16 array, dimension (LDB,NRHS)
*>     On entry, the N-by-NRHS right hand side matrix B.
*>     On exit,
*>     if EQUED = 'N', B is not modified;
*>     if TRANS = 'N' and EQUED = 'R' or 'B', B is overwritten by
*>        diag(R)*B;
*>     if TRANS = 'T' or 'C' and EQUED = 'C' or 'B', B is
*>        overwritten by diag(C)*B.
*> \endverbatim
*>
*> \param[in] LDB
*> \verbatim
*>          LDB is INTEGER
*>     The leading dimension of the array B.  LDB >= max(1,N).
*> \endverbatim
*>
*> \param[out] X
*> \verbatim
*>          X is COMPLEX*16 array, dimension (LDX,NRHS)
*>     If INFO = 0, the N-by-NRHS solution matrix X to the original
*>     system of equations.  Note that A and B are modified on exit
*>     if EQUED .ne. 'N', and the solution to the equilibrated system is
*>     inv(diag(C))*X if TRANS = 'N' and EQUED = 'C' or 'B', or
*>     inv(diag(R))*X if TRANS = 'T' or 'C' and EQUED = 'R' or 'B'.
*> \endverbatim
*>
*> \param[in] LDX
*> \verbatim
*>          LDX is INTEGER
*>     The leading dimension of the array X.  LDX >= max(1,N).
*> \endverbatim
*>
*> \param[out] RCOND
*> \verbat