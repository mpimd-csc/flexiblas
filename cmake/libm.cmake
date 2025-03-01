include(CheckFunctionExists)

SET(LIBM_LIST m libm)
if(NOT COS_FUNCTION_EXISTS AND NOT NEED_LINKING_AGAINST_LIBM)
  CHECK_FUNCTION_EXISTS(cos COS_FUNCTION_EXISTS)

  if(NOT COS_FUNCTION_EXISTS)
      foreach(LIBM_CANDIDATE IN LISTS LIBM_LIST)
          MESSAGE(STATUS "Check if cos needs ${LIBM_CANDIDATE}")
          unset(COS_FUNCTION_EXISTS CACHE)
          SET(CMAKE_REQUIRED_LIBRARIES_BACKUP ${CMAKE_REQUIRED_LIBRARIES})
          list(APPEND CMAKE_REQUIRED_LIBRARIES ${LIBM_CANDIDATE})
          CHECK_FUNCTION_EXISTS(cos COS_FUNCTION_EXISTS)
          if(COS_FUNCTION_EXISTS)
              set(NEED_LINKING_AGAINST_LIBM True CACHE BOOL "" FORCE)
              set(LIBM_NAME ${LIBM_CANDIDATE} CACHE STRING "Name of the libm implementation" FORCE)
              break()
          endif()
          set(CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES_BACKUP})
      endforeach()
      if (NOT NEED_LINKING_AGAINST_LIBM)
          MESSAGE(FATAL_ERROR "libm required but not found.")
      endif()
  endif()
endif()

