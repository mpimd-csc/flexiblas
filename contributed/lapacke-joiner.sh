#!/usr/bin/env sh
#
# This is a helper to generate the joined files for CBLAS and LAPACKE
#
cwd=$(pwd)
for i in lapacke*
do
    cd "${cwd}"
    if [ ! -d $i ]; then
        continue
    fi
    echo "Joining LAPACKE ${i}"
    cd "${i}"
    rm -f lapacke_joined.c
    cd src
    for i in *.c ;
    do
        cat $i >> ../lapacke_joined.c
        echo "" >> ../lapacke_joined.c
    done
done

# CBLAS
cd "${cwd}"
cd cblas

for i in *.c ;
do
    if [ "$i" == "cblas_joined.c" ]; then
        continue
    fi
    cat $i >> ../cblas_joined.c
    echo "" >> ../cblas_joined.c
done
mv ../cblas_joined.c .

