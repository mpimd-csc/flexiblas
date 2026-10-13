FlexiBLAS Codegen
=================

The code generate creates the interfaces for

 * BLAS
 * LAPACK

and works on top of Python. Therefore the following steps are required.


1. Preparation
```shell
python3 -m venv env
source env/bin/activate
pip install -r requirements.txt
```
2. Extract YAML from LAPACK sources
```shell
bash build-lapack.sh
python3 ExtractFortran.py
python3 CheckLapackSymbols.py
```
3. (Re-)Generate the BLAS/LAPACK etc Wrappers
```shell
python3 GenerateBLAS.py
python3 GenerateCBLAS.py
python3 GenerateLAPACKE.py
```
