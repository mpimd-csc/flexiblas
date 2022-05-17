! LIBCSCUTILS - Fortran Interface to LUA
! Copyright (C) Martin Koehler, 2020
!
! This library is free software; you can redistribute it and/or modify
! it under the terms of the GNU Lesser General Public License as published
! by the Free Software Foundation; either version 2.1 of the License, or
! (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
! GNU Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License
! along with this library; if not, see <http://www.gnu.org/licenses/>.
!

!> @defgroup lua_fortran Fortran Interface for LUA
!> @ingroup lua
!>
!> @brief The CSC_LUA Fortran modules contains the interface to access the LUA
!> functionality from libcscutils out of Fortran 2003+ codes.
!>
!> The CSC_LUA Fortran module contains the interfaces to access LUA scripts from
!> Fortran. The interface wraps around the C functions given in \ref lua "here".
!>
!> All Fortran subroutines have a last argument, named \b INFO which is used as return value
!> to indicate a successful operation. If INFO is zero the subroutine works without an error.
!> All other values indicate an error if not stated differently in the documentation
!> of a subroutine.


!> @addtogroup lua_fortran
!> @{

!> @brief LUA Binding for Fortran
!>
!> The CSC_LUA module provides the bindings for the LUA interface for Fortran.
!> @see lua_fortran
MODULE CSC_LUA
    USE ISO_C_BINDING
    IMPLICIT NONE

    INTERFACE
        FUNCTION CSC_LUA_INIT_C() BIND(C, name="csc_lua_init")
            USE, INTRINSIC :: ISO_C_BINDING
            IMPORT
            TYPE(C_PTR) :: CSC_LUA_INIT_C
        END FUNCTION

        SUBROUTINE CSC_LUA_FINALIZE_C( LUA ) BIND(C, name="csc_lua_finalize")
            USE, INTRINSIC :: ISO_C_BINDING
            IMPORT
            TYPE(C_PTR), INTENT(IN), VALUE :: LUA
        END SUBROUTINE

        FUNCTION CSC_LUA_LOADFILE_C(LUA, FILENAME) BIND(C, name="csc_lua_loadfile")
            USE, INTRINSIC :: ISO_C_BINDING
            IMPORT
            TYPE(C_PTR), INTENT(IN), VALUE :: LUA
            CHARACTER(KIND=C_CHAR), INTENT(IN)     :: FILENAME
            INTEGER(KIND = C_INT) :: CSC_LUA_LOADFILE_C
        END FUNCTION

        FUNCTION CSC_LUA_LOADSTRING_C(LUA, STRING) BIND(C, name="csc_lua_loadstring")
            USE, INTRINSIC :: ISO_C_BINDING
            IMPORT
            TYPE(C_PTR), INTENT(IN), VALUE :: LUA
            CHARACTER(KIND = C_CHAR, LEN=1), DIMENSION(*), INTENT(IN) ::  STRING
            INTEGER(KIND = C_INT) :: CSC_LUA_LOADSTRING_C
        END FUNCTION


        FUNCTION CSC_LUA_RUN_C( LUA ) BIND(C, name="csc_lua_run")
            USE, INTRINSIC :: ISO_C_BINDING
            IMPORT
            TYPE(C_PTR), INTENT(IN), VALUE :: LUA
            INTEGER(KIND = C_INT) :: CSC_LUA_RUN_C
        END FUNCTION

        FUNCTION CSC_LUA_GLOBAL_INT_C(LUA, GNAME, VAL) BIND(C, name="csc_lua_global_int")
            USE, INTRINSIC :: ISO_C_BINDING
            IMPORT
            TYPE(C_PTR), INTENT(IN), VALUE :: LUA
            CHARACTER(KIND=C_CHAR), INTENT(IN)     :: GNAME
            INTEGER(KIND = C_INT), INTENT(IN), VALUE :: VAL
            INTEGER(KIND = C_INT) :: CSC_LUA_GLOBAL_INT_C
        END FUNCTION

        FUNCTION CSC_LUA_GLOBAL_DOUBLE_C(LUA, GNAME, VAL) BIND(C, name="csc_lua_global_double")
            USE, INTRINSIC :: ISO_C_BINDING
            IMPORT
            TYPE(C_PTR), INTENT(IN), VALUE :: LUA
            CHARACTER(KIND=C_CHAR), INTENT(IN)     :: GNAME
            REAL(KIND = C_DOUBLE), INTENT(IN), VALUE :: VAL
            INTEGER(KIND = C_INT) :: CSC_LUA_GLOBAL_DOUBLE_C
        END FUNCTION

        FUNCTION CSC_LUA_GLOBAL_STRING_C(LUA, GNAME, VAL) BIND(C, name="csc_lua_global_string")
            USE, INTRINSIC :: ISO_C_BINDING
            IMPORT
            TYPE(C_PTR), INTENT(IN), VALUE :: LUA
            CHARACTER(KIND=C_CHAR), INTENT(IN)     :: GNAME
            CHARACTER(KIND=C_CHAR), INTENT(IN)     :: VAL
            INTEGER(KIND = C_INT) :: CSC_LUA_GLOBAL_STRING_C
        END FUNCTION

        FUNCTION CSC_LUA_FUNCTION_EXISTS_C(LUA, FNAME) BIND(C, name="csc_lua_function_exists")
            USE, INTRINSIC :: ISO_C_BINDING
            IMPORT
            TYPE(C_PTR), INTENT(IN), VALUE :: LUA
            CHARACTER(KIND=C_CHAR), INTENT(IN)     :: FNAME
            INTEGER(KIND = C_INT) :: CSC_LUA_FUNCTION_EXISTS_C
        END FUNCTION

        FUNCTION CSC_LUA_CALL_ARG_0_RET_0_C( LUA , FNAME )  BIND(C, name="csc_lua_call_arg_0_ret_0")
            USE, INTRINSIC :: ISO_C_BINDING
            IMPORT
            TYPE(C_PTR), INTENT(IN), VALUE :: LUA
            CHARACTER(KIND=C_CHAR), INTENT(IN)     :: FNAME
            INTEGER(KIND = C_INT) :: CSC_LUA_CALL_ARG_0_RET_0_C
        END FUNCTION

        FUNCTION CSC_LUA_CALL_ARG_0_RET_I_C( LUA , FNAME, R1 )  BIND(C, name="csc_lua_call_arg_0_ret_i")
            USE, INTRINSIC :: ISO_C_BINDING
            IMPORT
            TYPE(C_PTR), INTENT(IN), VALUE :: LUA
            CHARACTER(KIND=C_CHAR), INTENT(IN)     :: FNAME
            INTEGER(KIND = C_INT), INTENT(OUT) :: R1
            INTEGER(KIND = C_INT) :: CSC_LUA_CALL_ARG_0_RET_I_C
        END FUNCTION

        FUNCTION CSC_LUA_CALL_ARG_0_RET_II_C( LUA , FNAME, R1, R2 )  BIND(C, name="csc_lua_call_arg_0_ret_ii")
            USE, INTRINSIC :: ISO_C_BINDING
            IMPORT
            TYPE(C_PTR), INTENT(IN), VALUE :: LUA
            CHARACTER(KIND=C_CHAR), INTENT(IN)     :: FNAME
            INTEGER(KIND = C_INT), INTENT(OUT) :: R1
            INTEGER(KIND = C_INT), INTENT(OUT) :: R2
            INTEGER(KIND = C_INT) :: CSC_LUA_CALL_ARG_0_RET_II_C
        END FUNCTION

        FUNCTION CSC_LUA_CALL_ARG_I_RET_I_C( LUA , FNAME, I1, R1 )  BIND(C, name="csc_lua_call_arg_i_ret_i")
            USE, INTRINSIC :: ISO_C_BINDING
            IMPORT
            TYPE(C_PTR), INTENT(IN), VALUE :: LUA
            CHARACTER(KIND=C_CHAR), INTENT(IN)     :: FNAME
            INTEGER(KIND = C_INT), INTENT(IN), VALUE :: I1
            INTEGER(KIND = C_INT), INTENT(OUT) :: R1
            INTEGER(KIND = C_INT) :: CSC_LUA_CALL_ARG_I_RET_I_C
        END FUNCTION

        FUNCTION CSC_LUA_CALL_ARG_II_RET_I_C( LUA , FNAME, I1, I2, R1 )  BIND(C, name="csc_lua_call_arg_ii_ret_i")
            USE, INTRINSIC :: ISO_C_BINDING
            IMPORT
            TYPE(C_PTR), INTENT(IN), VALUE :: LUA
            CHARACTER(KIND=C_CHAR), INTENT(IN)     :: FNAME
            INTEGER(KIND = C_INT), INTENT(IN), VALUE :: I1
            INTEGER(KIND = C_INT), INTENT(IN), VALUE :: I2
            INTEGER(KIND = C_INT), INTENT(OUT) :: R1
            INTEGER(KIND = C_INT) :: CSC_LUA_CALL_ARG_II_RET_I_C
        END FUNCTION

        FUNCTION CSC_LUA_CALL_ARG_III_RET_I_C( LUA , FNAME, I1, I2, I3, R1 )  BIND(C, name="csc_lua_call_arg_iii_ret_i")
            USE, INTRINSIC :: ISO_C_BINDING
            IMPORT
            TYPE(C_PTR), INTENT(IN), VALUE :: LUA
            CHARACTER(KIND=C_CHAR), INTENT(IN)     :: FNAME
            INTEGER(KIND = C_INT), INTENT(IN), VALUE :: I1
            INTEGER(KIND = C_INT), INTENT(IN), VALUE :: I2
            INTEGER(KIND = C_INT), INTENT(IN), VALUE :: I3
            INTEGER(KIND = C_INT), INTENT(OUT) :: R1
            INTEGER(KIND = C_INT) :: CSC_LUA_CALL_ARG_III_RET_I_C
        END FUNCTION

        FUNCTION CSC_LUA_CALL_ARG_SII_RET_I_C( LUA , FNAME, I1, I2, I3, R1 )  BIND(C, name="csc_lua_call_arg_sii_ret_i")
            USE, INTRINSIC :: ISO_C_BINDING
            IMPORT
            TYPE(C_PTR), INTENT(IN), VALUE :: LUA
            CHARACTER(KIND=C_CHAR), INTENT(IN)     :: FNAME
            CHARACTER(KIND=C_CHAR), INTENT(IN)     :: I1
            INTEGER(KIND = C_INT), INTENT(IN), VALUE :: I2
            INTEGER(KIND = C_INT), INTENT(IN), VALUE :: I3
            INTEGER(KIND = C_INT), INTENT(OUT) :: R1
            INTEGER(KIND = C_INT) :: CSC_LUA_CALL_ARG_SII_RET_I_C
        END FUNCTION

    END INTERFACE

    !> @brief Set global values in the LUA interpreter
    !>
    !> The CSC_LUA_GLOBAL interface provides the overloaded
    !> subroutines to set INTEGER, DOUBLE PRECISION, or STRING values
    !> globally in the LUA interpreter.
   INTERFACE CSC_LUA_GLOBAL
        MODULE PROCEDURE CSC_LUA_GLOBAL_INT
        MODULE PROCEDURE CSC_LUA_GLOBAL_DOUBLE
        MODULE PROCEDURE CSC_LUA_GLOBAL_STRING
    END INTERFACE

    PRIVATE :: CSC_LUA_GLOBAL_INT
    PRIVATE :: CSC_LUA_GLOBAL_DOUBLE
    PRIVATE :: CSC_LUA_GLOBAL_STRING

    !> @brief Call a LUA function returning a single INTEGER value
    !>
    !> The CSC_LUA_CALL_RETURN_INT calls a LUA function which returns a single integer
    !> value.
    INTERFACE CSC_LUA_CALL_RETURN_INT
        MODULE PROCEDURE CSC_LUA_CALL_ARG_0_RET_I
        MODULE PROCEDURE CSC_LUA_CALL_ARG_I_RET_I
        MODULE PROCEDURE CSC_LUA_CALL_ARG_II_RET_I
        MODULE PROCEDURE CSC_LUA_CALL_ARG_III_RET_I
        MODULE PROCEDURE CSC_LUA_CALL_ARG_SII_RET_I
    END INTERFACE

    PRIVATE :: CSC_LUA_CALL_ARG_0_RET_I
    PRIVATE :: CSC_LUA_CALL_ARG_I_RET_I
    PRIVATE :: CSC_LUA_CALL_ARG_II_RET_I
    PRIVATE :: CSC_LUA_CALL_ARG_III_RET_I
    PRIVATE :: CSC_LUA_CALL_ARG_SII_RET_I

    !> @brief Call a LUA function without a return value
    !>
    !> The CSC_LUA_CALL_RETURN_NULL interface calls a LUA function, which
    !> does not return anything.
    !>
    INTERFACE CSC_LUA_CALL_RETURN_NULL
        MODULE PROCEDURE CSC_LUA_CALL_ARG_0_RET_0
    END INTERFACE

    PRIVATE :: CSC_LUA_CALL_ARG_0_RET_0

    !> @brief Call a LUA function with two integer return values
    !>
    !> The CSC_LUA_CALL_RETURN_2INT interfaces provides access to
    !> LUA functions, which return two integer values.
    !>
    INTERFACE CSC_LUA_CALL_RETURN_2INT
        MODULE PROCEDURE CSC_LUA_CALL_ARG_0_RET_II
    END INTERFACE

    PRIVATE :: CSC_LUA_CALL_ARG_0_RET_II


CONTAINS

    !> @brief Initialize a LUA interpreter
    !> @param[out]      LUA     C-Pointer to the LUA interpreter
    !> @param[out]      INFO    Status code
    !>
    !> The CSC_LUA_INIT subroutine initializes a LUA interpreter.
    !>
    !> The INFO argument returns 0 on success, 1 otherwise.
    !>
    SUBROUTINE CSC_LUA_INIT( LUA , INFO )
        IMPLICIT NONE
        TYPE(C_PTR), INTENT(OUT)  :: LUA
        INTEGER, INTENT(OUT) :: INFO

        INFO = 0
        LUA = CSC_LUA_INIT_C()

        IF ( .NOT. C_ASSOCIATED(LUA)) INFO = 1

        RETURN
    END SUBROUTINE CSC_LUA_INIT

    !> @brief Clears a LUA interpreter
    !> @param[in]   LUA     C-Pointer to the LUA interpreter
    !> @param[out]  INFO    Status code
    !>
    !> The CSC_LUA_FINALIZE subroutine clears a LUA interpreter addressed
    !> by LUA.
    !>
    !> The INFO argument always returns 0 and only exists for compatibility reasons.
    !>
    SUBROUTINE CSC_LUA_FINALIZE( LUA, INFO )
        IMPLICIT NONE
        TYPE(C_PTR), INTENT(IN)  :: LUA
        INTEGER, INTENT(OUT) :: INFO

        INFO = 0

        IF (C_ASSOCIATED(LUA) ) THEN
            CALL CSC_LUA_FINALIZE_C(LUA)
        END IF
        RETURN
    END SUBROUTINE CSC_LUA_FINALIZE

    !> @brief Load a LUA script into the interpreter
    !> @param[inout]   LUA      C-Pointer to the LUA interpreter
    !> @param[in]   FILENAME    File to load.
    !> @param[out]  INFO    Status code
    !>
    !> The CSC_LUA_LOADFILE subroutine loads a LUA file into the LUA interpreter.
    !> The file is not executed immediately. This requires a separate call to \ref csc_lua_run "CSC_LUA_RUN."
    !>
    !> The INFO argument returns:
    !> @li 0, on success
    !> @li 1, if the LUA object is not initialized.
    !> @li 2, if the loading of the file failed.
    !>
    SUBROUTINE CSC_LUA_LOADFILE( LUA, FILENAME, INFO )
        IMPLICIT NONE
        TYPE(C_PTR), INTENT(INOUT)  :: LUA
        CHARACTER(LEN=*), INTENT(IN) :: FILENAME
        INTEGER, INTENT(OUT) :: INFO

        INTEGER(KIND = C_INT) ::  RET

        INFO = 0

        IF (.NOT. C_ASSOCIATED(LUA) ) THEN
            INFO = 1
            RETURN
        END IF

        RET = CSC_LUA_LOADFILE_C(LUA, FILENAME // C_NULL_CHAR)

        IF ( RET.NE. 0 ) THEN
            INFO = 2
            RETURN
        END IF

        RETURN
    END SUBROUTINE CSC_LUA_LOADFILE

    !> @brief Load a LUA script from a Fortran string.
    !> @param[inout]    LUA     C-Pointer to the LUA interpreter
    !> @param[in]       STRING  Fortran string containing the LUA code
    !> @param[out]      INFO    Status code
    !>
    !> The CSC_LUA_LOADSTRING_FORTRAN subroutine loads a LUA script form a
    !> Fortran style string. The code is not executed immediately. Therefore,
    !> a subsequent call to \ref csc_lua_run is required.
    !>
    !> @see csc_lua::csc_lua_loadstring
    !> @see csc_lua::csc_lua_loadstring::csc_lua_loadstring_cptr
    !>
    SUBROUTINE CSC_LUA_LOADSTRING_FORTRAN(LUA, STRING, INFO)
        IMPLICIT NONE
        TYPE(C_PTR), INTENT(INOUT)  :: LUA
        CHARACTER(LEN=*), INTENT(IN) :: STRING
        INTEGER, INTENT(OUT) :: INFO

        INTEGER(KIND = C_INT) ::  RET

        INFO = 0

        IF (.NOT. C_ASSOCIATED(LUA) ) THEN
            INFO = 1
            RETURN
        END IF

        RET = CSC_LUA_LOADSTRING_C(LUA, STRING // C_NULL_CHAR)

        IF ( RET.NE. 0 ) THEN
            INFO = 2
            RETURN
        END IF

        RETURN
    END SUBROUTINE CSC_LUA_LOADSTRING_FORTRAN



    !> @brief Execute the loaded LUA scripts
    !> @param[inout]   LUA      C-Pointer to the LUA interpreter
    !> @param[out]  INFO    Status code
    !>
    !> The CSC_LUA_RUN subroutine executes the loaded LUA script.
    !>
    !> The INFO argument returns:
    !> @li 0, on success
    !> @li 1, if the LUA object is not initialized.
    !> @li 2, if the executing fails.
    !>
    SUBROUTINE CSC_LUA_RUN( LUA, INFO )
        IMPLICIT NONE
        TYPE(C_PTR), INTENT(INOUT)  :: LUA
        INTEGER, INTENT(OUT) :: INFO

        INTEGER(KIND = C_INT) ::  RET

        INFO = 0

        IF (.NOT. C_ASSOCIATED(LUA) ) THEN
            INFO = 1
            RETURN
        END IF

        RET = CSC_LUA_RUN_C(LUA)
        IF ( RET .NE. 0 ) THEN
            INFO = 2
            RETURN
        END IF
        RETURN
    END SUBROUTINE CSC_LUA_RUN


    !> @brief Set a global variable in the LUA interpreter (INTEGER version).
    !> @param[inout]   LUA      C-Pointer to the LUA interpreter
    !> @param[in]      NAME     Name of the global variable
    !> @param[in]      VAL      Value of the global variable
    !> @param[out]  INFO    Status code
    !>
    !> The CSC_LUA_GLOBAL_INT subroutine sets a global variable in the LUA interpreter.
    !> The value must be an integer.
    !>
    !> The INFO argument returns:
    !> @li 0, on success
    !> @li 1, if the LUA object is not initialized.
    !> @li 2, if the variable could not be set.
    !>
    !> @see csc_lua::csc_lua_global::csc_lua_global_double
    !> @see csc_lua::csc_lua_global::csc_lua_global_string
    !> @see csc_lua::csc_lua_global
    !>
    SUBROUTINE CSC_LUA_GLOBAL_INT( LUA, NAME, VAL, INFO)
        IMPLICIT NONE
        TYPE(C_PTR), INTENT(INOUT)  :: LUA
        CHARACTER(LEN = *), INTENT(IN) :: NAME
        INTEGER, INTENT(IN) :: VAL
        INTEGER, INTENT(OUT) :: INFO

        INTEGER(KIND = C_INT) ::  RET
        INTEGER(KIND = C_INT) ::  CVAL
        INFO = 0

        IF (.NOT. C_ASSOCIATED(LUA) ) THEN
            INFO = 1
            RETURN
        END IF

        CVAL = INT ( VAL, KIND = C_INT)

        RET = CSC_LUA_GLOBAL_INT_C( LUA, NAME // C_NULL_CHAR, CVAL)

        IF ( RET .NE. 0 ) THEN
            INFO = 2
            RETURN
        END IF
        RETURN
    END SUBROUTINE CSC_LUA_GLOBAL_INT

    !> @brief Set a global variable in the LUA interpreter (DOUBLE PRECISION version).
    !> @param[inout]   LUA      C-Pointer to the LUA interpreter
    !> @param[in]      NAME     Name of the global variable
    !> @param[in]      VAL      Value of the global variable
    !> @param[out]  INFO    Status code
    !>
    !> The CSC_LUA_GLOBAL_DOUBLE subroutine sets a global variable in the LUA interpreter.
    !> The value must be a double precision value.
    !>
    !> The INFO argument returns:
    !> @li 0, on success
    !> @li 1, if the LUA object is not initialized.
    !> @li 2, if the variable could not be set.
    !>
    !> @see csc_lua::csc_lua_global::csc_lua_global_int
    !> @see csc_lua::csc_lua_global::csc_lua_global_string
    !> @see csc_lua::csc_lua_global
    !>
    SUBROUTINE CSC_LUA_GLOBAL_DOUBLE( LUA, NAME, VAL, INFO)
        IMPLICIT NONE
        TYPE(C_PTR), INTENT(INOUT)  :: LUA
        CHARACTER(LEN = *), INTENT(IN) :: NAME
        DOUBLE PRECISION, INTENT(IN) :: VAL
        INTEGER, INTENT(OUT) :: INFO

        INTEGER(KIND = C_INT) ::  RET
        INFO = 0

        IF (.NOT. C_ASSOCIATED(LUA) ) THEN
            INFO = 1
            RETURN
        END IF

        RET = CSC_LUA_GLOBAL_DOUBLE_C( LUA, NAME // C_NULL_CHAR, VAL)

        IF ( RET .NE. 0 ) THEN
            INFO = 2
            RETURN
        END IF
        RETURN
    END SUBROUTINE CSC_LUA_GLOBAL_DOUBLE

    !> @brief Set a global variable in the LUA interpreter (STRING version).
    !> @param[inout]   LUA      C-Pointer to the LUA interpreter
    !> @param[in]      NAME     Name of the global variable
    !> @param[in]      VAL      Value of the global variable
    !> @param[out]  INFO    Status code
    !>
    !> The CSC_LUA_GLOBAL_STRING subroutine sets a global variable in the LUA interpreter.
    !> The value must be a character array. The terminating C_NULL_CHAR is automatically added.
    !>
    !> The INFO argument returns:
    !> @li 0, on success
    !> @li 1, if the LUA object is not initialized.
    !> @li 2, if the variable could not be set.
    !>
    !> @see csc_lua::csc_lua_global::csc_lua_global_int
    !> @see csc_lua::csc_lua_global::csc_lua_global_double
    !> @see csc_lua::csc_lua_global
    !>
    SUBROUTINE CSC_LUA_GLOBAL_STRING( LUA, NAME, VAL, INFO)
        IMPLICIT NONE
        TYPE(C_PTR), INTENT(INOUT)  :: LUA
        CHARACTER(LEN = *), INTENT(IN) :: NAME
        CHARACTER(LEN = *), INTENT(IN) :: VAL
        INTEGER, INTENT(OUT) :: INFO

        INTEGER(KIND = C_INT) ::  RET
        INFO = 0

        IF (.NOT. C_ASSOCIATED(LUA) ) THEN
            INFO = 1
            RETURN
        END IF

        RET = CSC_LUA_GLOBAL_STRING_C( LUA, NAME // C_NULL_CHAR, VAL // C_NULL_CHAR)

        IF ( RET .NE. 0 ) THEN
            INFO = 2
            RETURN
        END IF
        RETURN
    END SUBROUTINE CSC_LUA_GLOBAL_STRING


    !> @brief Call a LUA function with an integer return value and zero arguments.
    !> @param[inout]    LUA     C-Pointer to the LUA interpreter
    !> @param[in]       NAME    Function name
    !> @param[out]      R1      Return value
    !> @param[out]      INFO    Status code
    !>
    !> The CSC_LUA_CALL_ARG_0_RET_I subroutine calls a LUA function which takes no input
    !> and returns a single integer value.
    !>
    !> The INFO argument returns:
    !> @li 0, on success
    !> @li 1, if the LUA object is not initialized.
    !> @li 2, if the function executions fails.
    !>
    !> @see csc_lua::csc_lua_call_return_int::csc_lua_call_arg_i_ret_i
    !> @see csc_lua::csc_lua_call_return_int::csc_lua_call_arg_ii_ret_i
    !> @see csc_lua::csc_lua_call_return_int::csc_lua_call_arg_iii_ret_i
    !> @see csc_lua::csc_lua_call_return_int::csc_lua_call_arg_sii_ret_i
    !> @see csc_lua::csc_lua_call_return_int
    !>
    SUBROUTINE CSC_LUA_CALL_ARG_0_RET_I( LUA, NAME, R1, INFO)
        IMPLICIT NONE
        TYPE(C_PTR), INTENT(INOUT) :: LUA
        CHARACTER(LEN=*), INTENT(IN) :: NAME
        INTEGER, INTENT(OUT):: R1
        INTEGER, INTENT(OUT) :: INFO

        INTEGER(KIND = C_INT) :: CR1
        INTEGER(KIND = C_INT) :: RET

        INFO = 0

        IF (.NOT. C_ASSOCIATED(LUA) ) THEN
            INFO = 1
            RETURN
        END IF


        RET = CSC_LUA_CALL_ARG_0_RET_I_C( LUA, NAME // C_NULL_CHAR,  CR1)

        R1 = INT(CR1)

        IF ( RET .NE. 0 ) THEN
            INFO = 2
        END IF
        RETURN
    END SUBROUTINE

    !> @brief Call a LUA function with an integer return value and a integer input argument.
    !> @param[inout]    LUA     C-Pointer to the LUA interpreter
    !> @param[in]       NAME    Function name
    !> @param[in]       I1      Integer input value
    !> @param[out]      R1      Return value
    !> @param[out]      INFO    Status code
    !>
    !> The CSC_LUA_CALL_ARG_I_RET_I subroutine calls a LUA function which takes a single integer
    !> input argument and returns a single integer value.
    !>
    !> The INFO argument returns:
    !> @li 0, on success
    !> @li 1, if the LUA object is not initialized.
    !> @li 2, if the function executions fails.
    !>
    !> @see csc_lua::csc_lua_call_return_int::csc_lua_call_arg_0_ret_i
    !> @see csc_lua::csc_lua_call_return_int::csc_lua_call_arg_ii_ret_i
    !> @see csc_lua::csc_lua_call_return_int::csc_lua_call_arg_iii_ret_i
    !> @see csc_lua::csc_lua_call_return_int::csc_lua_call_arg_sii_ret_i
    !> @see csc_lua::csc_lua_call_return_int
    !>
    SUBROUTINE CSC_LUA_CALL_ARG_I_RET_I( LUA, NAME, I1, R1, INFO)
        IMPLICIT NONE
        TYPE(C_PTR), INTENT(INOUT) :: LUA
        CHARACTER(LEN=*), INTENT(IN) :: NAME
        INTEGER, INTENT(IN) :: I1
        INTEGER, INTENT(OUT):: R1
        INTEGER, INTENT(OUT) :: INFO

        INTEGER(KIND = C_INT) :: CI1, CR1
        INTEGER(KIND = C_INT) :: RET

        INFO = 0

        IF (.NOT. C_ASSOCIATED(LUA) ) THEN
            INFO = 1
            RETURN
        END IF

        CI1 = INT(I1, KIND = C_INT)

        RET = CSC_LUA_CALL_ARG_I_RET_I_C( LUA, NAME // C_NULL_CHAR, CI1, CR1)

        R1 = INT(CR1)

        IF ( RET .NE. 0 ) THEN
            INFO = 2
        END IF
        RETURN
    END SUBROUTINE

    !> @brief Call a LUA function with an integer return value and two integer input arguments.
    !> @param[inout]    LUA     C-Pointer to the LUA interpreter
    !> @param[in]       NAME    Function name
    !> @param[in]       I1      First integer input value
    !> @param[in]       I2      Second integer input value
    !> @param[out]      R1      Return value
    !> @param[out]      INFO    Status code
    !>
    !> The CSC_LUA_CALL_ARG_II_RET_I subroutine calls a LUA function which takes two integer
    !> input arguments and returns a single integer value.
    !>
    !> The INFO argument returns:
    !> @li 0, on success
    !> @li 1, if the LUA object is not initialized.
    !> @li 2, if the function executions fails.
    !>
    !> @see csc_lua::csc_lua_call_return_int::csc_lua_call_arg_0_ret_i
    !> @see csc_lua::csc_lua_call_return_int::csc_lua_call_arg_i_ret_i
    !> @see csc_lua::csc_lua_call_return_int::csc_lua_call_arg_iii_ret_i
    !> @see csc_lua::csc_lua_call_return_int::csc_lua_call_arg_sii_ret_i
    !> @see csc_lua::csc_lua_call_return_int
    !>
    SUBROUTINE CSC_LUA_CALL_ARG_II_RET_I( LUA, NAME, I1, I2, R1, INFO)
        IMPLICIT NONE
        TYPE(C_PTR), INTENT(INOUT) :: LUA
        CHARACTER(LEN=*), INTENT(IN) :: NAME
        INTEGER, INTENT(IN) :: I1, I2
        INTEGER, INTENT(OUT):: R1
        INTEGER, INTENT(OUT) :: INFO

        INTEGER(KIND = C_INT) :: CI1, CI2, CR1
        INTEGER(KIND = C_INT) :: RET

        INFO = 0

        IF (.NOT. C_ASSOCIATED(LUA) ) THEN
            INFO = 1
            RETURN
        END IF

        CI1 = INT(I1, KIND = C_INT)
        CI2 = INT(I2, KIND = C_INT)


        RET = CSC_LUA_CALL_ARG_II_RET_I_C( LUA, NAME // C_NULL_CHAR, CI1, CI2, CR1)

        R1 = INT(CR1)

        IF ( RET .NE. 0 ) THEN
            INFO = 2
        END IF
        RETURN
    END SUBROUTINE

    !> @brief Call a LUA function with an integer return value and three integer input arguments.
    !> @param[inout]    LUA     C-Pointer to the LUA interpreter
    !> @param[in]       NAME    Function name
    !> @param[in]       I1      First integer input value
    !> @param[in]       I2      Second integer input value
    !> @param[in]       I3      Third integer input value
    !> @param[out]      R1      Return value
    !> @param[out]      INFO    Status code
    !>
    !> The CSC_LUA_CALL_ARG_III_RET_I subroutine calls a LUA function which takes three integer
    !> input arguments and returns a single integer value.
    !>
    !> The INFO argument returns:
    !> @li 0, on success
    !> @li 1, if the LUA object is not initialized.
    !> @li 2, if the function executions fails.
    !>
    !> @see csc_lua::csc_lua_call_return_int::csc_lua_call_arg_0_ret_i
    !> @see csc_lua::csc_lua_call_return_int::csc_lua_call_arg_i_ret_i
    !> @see csc_lua::csc_lua_call_return_int::csc_lua_call_arg_ii_ret_i
    !> @see csc_lua::csc_lua_call_return_int::csc_lua_call_arg_sii_ret_i
    !> @see csc_lua::csc_lua_call_return_int
    !>
    SUBROUTINE CSC_LUA_CALL_ARG_III_RET_I( LUA, NAME, I1, I2, I3, R1, INFO)
        IMPLICIT NONE
        TYPE(C_PTR), INTENT(INOUT) :: LUA
        CHARACTER(LEN=*), INTENT(IN) :: NAME
        INTEGER, INTENT(IN) :: I1, I2, I3
        INTEGER, INTENT(OUT):: R1
        INTEGER, INTENT(OUT) :: INFO

        INTEGER(KIND = C_INT) :: CI1, CI2, CI3,  CR1
        INTEGER(KIND = C_INT) :: RET

        INFO = 0

        IF (.NOT. C_ASSOCIATED(LUA) ) THEN
            INFO = 1
            RETURN
        END IF

        CI1 = INT(I1, KIND = C_INT)
        CI2 = INT(I2, KIND = C_INT)
        CI3 = INT(I3, KIND = C_INT)

        RET = CSC_LUA_CALL_ARG_III_RET_I_C( LUA, NAME // C_NULL_CHAR, CI1, CI2, CI3, CR1)

        R1 = INT(CR1)

        IF ( RET .NE. 0 ) THEN
            INFO = 2
        END IF
        RETURN
    END SUBROUTINE

    !> @brief Call a LUA function with an integer return value and a string and two integer input arguments.
    !> @param[inout]    LUA     C-Pointer to the LUA interpreter
    !> @param[in]       NAME    Function name
    !> @param[in]       I1      First string input value
    !> @param[in]       I2      Second integer input value
    !> @param[in]       I3      Third integer input value
    !> @param[out]      R1      Return value
    !> @param[out]      INFO    Status code
    !>
    !> The CSC_LUA_CALL_ARG_SII_RET_I subroutine calls a LUA function which takes a string
    !> and two integer input arguments and returns a single integer value.
    !>
    !> The INFO argument returns:
    !> @li 0, on success
    !> @li 1, if the LUA object is not initialized.
    !> @li 2, if the function executions fails.
    !>
    !> @see csc_lua::csc_lua_call_return_int::csc_lua_call_arg_0_ret_i
    !> @see csc_lua::csc_lua_call_return_int::csc_lua_call_arg_i_ret_i
    !> @see csc_lua::csc_lua_call_return_int::csc_lua_call_arg_ii_ret_i
    !> @see csc_lua::csc_lua_call_return_int::csc_lua_call_arg_iii_ret_i
    !> @see csc_lua::csc_lua_call_return_int
    !>
    SUBROUTINE CSC_LUA_CALL_ARG_SII_RET_I( LUA, NAME, I1, I2, I3, R1, INFO)
        IMPLICIT NONE
        TYPE(C_PTR), INTENT(INOUT) :: LUA
        CHARACTER(LEN=*), INTENT(IN) :: NAME
        CHARACTER(LEN=*), INTENT(IN) :: I1
        INTEGER, INTENT(IN) :: I2, I3
        INTEGER, INTENT(OUT):: R1
        INTEGER, INTENT(OUT) :: INFO

        INTEGER(KIND = C_INT) :: CI2, CI3,  CR1
        INTEGER(KIND = C_INT) :: RET

        INFO = 0

        IF (.NOT. C_ASSOCIATED(LUA) ) THEN
            INFO = 1
            RETURN
        END IF

        CI2 = INT(I2, KIND = C_INT)
        CI3 = INT(I3, KIND = C_INT)

        RET = CSC_LUA_CALL_ARG_SII_RET_I_C( LUA, NAME // C_NULL_CHAR, I1 // C_NULL_CHAR, CI2, CI3, CR1)

        R1 = INT(CR1)

        IF ( RET .NE. 0 ) THEN
            INFO = 2
        END IF
        RETURN
    END SUBROUTINE

    !> @brief Call a LUA function without return values or input arguments.
    !> @param[inout]    LUA     C-Pointer to the LUA interpreter
    !> @param[in]       NAME    Function name
    !> @param[out]      INFO    Status code
    !>
    !> The CSC_LUA_CALL_ARG_0_RET_0 subroutine calls a LUA function which has not
    !> input or output arguments.
    !>
    !> The INFO argument returns:
    !> @li 0, on success
    !> @li 1, if the LUA object is not initialized.
    !> @li 2, if the function executions fails.
    !>
    !> @see csc_lua::csc_lua_call_return_null
    !>
    SUBROUTINE CSC_LUA_CALL_ARG_0_RET_0( LUA, NAME, INFO)
        IMPLICIT NONE
        TYPE(C_PTR), INTENT(INOUT) :: LUA
        CHARACTER(LEN=*), INTENT(IN) :: NAME
        INTEGER, INTENT(OUT) :: INFO

        INTEGER(KIND = C_INT) :: RET

        INFO = 0

        IF (.NOT. C_ASSOCIATED(LUA) ) THEN
            INFO = 1
            RETURN
        END IF

        RET = CSC_LUA_CALL_ARG_0_RET_0_C( LUA, NAME // C_NULL_CHAR )

        IF ( RET .NE. 0 ) THEN
            INFO = 2
        END IF
        RETURN
    END SUBROUTINE

    !> @brief Call a LUA function with two integer return values and no inputs.
    !> @param[inout]    LUA     C-Pointer to the LUA interpreter
    !> @param[in]       NAME    Function name
    !> @param[out]      R1      First return value
    !> @param[out]      R2      Second return value
    !> @param[out]      INFO    Status code
    !>
    !> The CSC_LUA_CALL_ARG_0_RET_II subroutine calls a LUA function which has not
    !> input and two integer return values.
    !>
    !> The INFO argument returns:
    !> @li 0, on success
    !> @li 1, if the LUA object is not initialized.
    !> @li 2, if the function executions fails.
    !>
    !> @see csc_lua::csc_lua_call_return_2int
    !>
    SUBROUTINE CSC_LUA_CALL_ARG_0_RET_II( LUA, NAME, R1, R2, INFO)
        IMPLICIT NONE
        TYPE(C_PTR), INTENT(INOUT) :: LUA
        CHARACTER(LEN=*), INTENT(IN) :: NAME
        INTEGER, INTENT(OUT) :: R1, R2
        INTEGER, INTENT(OUT) :: INFO

        INTEGER(KIND = C_INT) :: RET
        INTEGER(KIND = C_INT) :: CR1
        INTEGER(KIND = C_INT) :: CR2

        INFO = 0

        IF (.NOT. C_ASSOCIATED(LUA) ) THEN
            INFO = 1
            RETURN
        END IF

        RET = CSC_LUA_CALL_ARG_0_RET_II_C( LUA, NAME // C_NULL_CHAR, CR1, CR2 )

        R1 = INT(CR1)
        R2 = INT(CR2)

        IF ( RET .NE. 0 ) THEN
            INFO = 2
        END IF

        RETURN
    END SUBROUTINE




END MODULE CSC_LUA
!> @}
