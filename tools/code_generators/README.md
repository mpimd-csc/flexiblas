Generators for the LAPACK interface
===================================

The directory contains the generator scripts for the LAPACK interface.

* generate_hook_profile_lapack.py - generates the lapack specific code in
			            src/hooks/profile/
* generate_lapack_dummy.py        - generates the dummy symbol look files in
                                    src/fallback_lapack/
* generate_lapack.py              - generates the LAPACK interface in
                                    src/lapack_inferface/
* indentation.vim                 - vim keyinput to indent a file correctly
* indent_all.sh                   - script to indent all files correctly,
                                    to be used from the root of FlexiBLAS
