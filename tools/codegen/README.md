FlexiBLAS Codegen
=================

The code generate creates the interfaces for 

 * BLAS
 * LAPACK

and works on top of Python. Therefore the following steps are required.


1. Preparation 
```shell
python -m venv env
source env/bin/activate 
pip install -r requirements.txt
jupyter nbconvert CheckLapackSymbols.ipynb --to script
jupyter nbconvert GenerateBLAS.ipynb --to script
```
2. Extract LAPACK sources
```shell
bash build-lapack.sh 
python ExtractFortran.py 
python CheckLapackSymbols.py
```
3. Generate the BLAS/LAPACK Wrappers
```shelll
python GenerateBLAS.py
```