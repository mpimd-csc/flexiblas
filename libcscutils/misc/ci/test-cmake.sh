#!/bin/bash

apt update
apt-get install -qq --yes gfortran libhdf5-dev zlib1g-dev libbz2-dev liblzma-dev

set -e

CWD=$(pwd)

for cm in /opt/cmake/*/bin/cmake
do
	cm_version=`${cm} --version | grep version  | cut -d" " -f 3`
	cm_build_dir=${CWD}/build-${cm_version}

	echo "Build for ${cm_version} in ${cm_build_dir}..."
	mkdir -p ${cm_build_dir}
	cd "${cm_build_dir}"
	${cm} ${CWD} -DDEBUG=ON
	make > build.log 2>&1
	make test

	cd "${CWD}"
done

