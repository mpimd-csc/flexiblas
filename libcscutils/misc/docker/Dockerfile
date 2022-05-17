FROM docker-registry.csc.mpi-magdeburg.mpg.de:5000/ci-images/base/ubuntu:focal
ARG cmakeversion=3.5.2
ADD scripts/*.sh /root/
RUN bash /root/build-cmake.sh $cmakeversion

