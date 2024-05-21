#!/bin/bash

if [ $# -ne 1 ]; then
    echo "Usage: $0 BASEVERSION"
fi

BASEVERSION=$1
LASTVERSION=-1

for PATCH in `seq 0 99`
do
    RET=$(curl -o /dev/null -Isw '%{http_code}\n' https://cmake.org/files/v${BASEVERSION}/cmake-${BASEVERSION}.${PATCH}.tar.gz )

    # echo "$PATCH  $RET"
    if [ $RET == 200 ]; then
        LASTVERSION=$BASEVERSION.$PATCH
    else
        break;
    fi
done
echo "${LASTVERSION}"

