# - Define macro to check restrict keyword
#
#  TEST_RESTRICT(VARIABLE)
#
#  VARIABLE will be set to the keyword
#
#  Remember to have a cmakedefine for it too...

MACRO(TEST_RESTRICT VARIABLE)
	IF(NOT DEFINED TEST_${VARIABLE})
		MESSAGE(STATUS "Checking for restrict keyword")
		#FOREACH (CSTD "-restrict" "-std=c11" "-std=c99" "-std=c90")
		#MESSAGE( "-> ${CSTD}" )
		# Start with __restrict__, since that is the C++ default keyword.
		FOREACH(KEYWORD "__restrict__" "__restrict" "restrict")
			IF(NOT TEST_${VARIABLE})
				TRY_COMPILE(TEST_${VARIABLE} "${CMAKE_BINARY_DIR}"
					"${CMAKE_SOURCE_DIR}/CMakeModules/TestRestrict.c"
					COMPILE_DEFINITIONS "-DTESTRESTRICTDEF=${KEYWORD} ${CSTD}" )
				SET(LAST_RESTRICT_KEYWORD ${KEYWORD})
			ENDIF(NOT TEST_${VARIABLE})
		ENDFOREACH(KEYWORD)
		#		ENDFOREACH(CSTD)

		IF(TEST_${VARIABLE})
			SET(${VARIABLE} ${LAST_RESTRICT_KEYWORD} CACHE INTERNAL "Restrict keyword" FORCE)
			MESSAGE(STATUS "Checking for restrict keyword - ${LAST_RESTRICT_KEYWORD}")
		ELSE(TEST_${VARIABLE})
			SET(${VARIABLE} " " CACHE INTERNAL "Restrict keyword" FORCE)
			MESSAGE(STATUS "Checking for restrict keyword - not found")
		ENDIF(TEST_${VARIABLE})
	ENDIF(NOT DEFINED TEST_${VARIABLE})        
ENDMACRO(TEST_RESTRICT VARIABLE)





