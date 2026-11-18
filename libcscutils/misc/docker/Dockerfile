FROM gitlab.mpi-magdeburg.mpg.de/ci-images/base/ubuntu:noble
ENV DEBIAN_FRONTEND=noninteractive
RUN sed -i -e 's,archive.ubuntu.com/ubuntu,ftp5.gwdg.de/pub/linux/debian/ubuntu,g' /etc/apt/sources.list.d/ubuntu.sources
RUN echo 'APT::Sandbox::User "root";' > /etc/apt/apt.conf.d/10sandbox
RUN apt update && \
    apt install -qq -y git wget libssl-dev build-essential gfortran make sed libarchive-dev zlib1g-dev libbz2-dev liblzma-dev

ARG cmakeversion=3.5.2
ADD scripts/*.sh /root/
RUN bash /root/build-cmake.sh $cmakeversion

