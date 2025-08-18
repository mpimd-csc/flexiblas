FROM gitlab.mpi-magdeburg.mpg.de/ci-images/base/ubuntu:focal
ARG cmakeversion=3.5.2
ADD scripts/*.sh /root/
RUN bash /root/build-cmake.sh $cmakeversion

