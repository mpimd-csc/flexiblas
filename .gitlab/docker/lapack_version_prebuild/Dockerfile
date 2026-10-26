FROM gitlab.mpi-magdeburg.mpg.de/ci-images/base/ubuntu:jammy
RUN apt update -y ;\
    apt dist-upgrade -y ;\
    apt install -y git build-essential wget cmake gcc gfortran make libblas-dev liblapack-dev \
	libopenblas-pthread-dev libopenblas-openmp-dev libopenblas-serial-dev \
	libblis-serial-dev libblis-pthread-dev libblis-openmp-dev \
	cmake libblas-dev libopenblas-dev libatlas-base-dev python3 python-is-python3 libsqlite3-dev libblis-dev ccache

WORKDIR /root
ADD scripts/*.sh /root/
ARG LAPACK_VERSION
RUN bash ./build-lapack.sh /opt/lapack/ $LAPACK_VERSION

