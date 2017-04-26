Name:           flexiblas
Version:        1.1.0
Release:        1
Summary:        The FlexiBLAS is a BLAS wrapper with runtime exchangeable backends. 
License:        GPLv3
Group:          Productivity/Scientific/Math
Url:            http://www.mpi-magdeburg.mpg.de/mpcsc/software/flexiblas
Source0:        http://www.mpi-magdeburg.mpg.de/mpcsc/software/flexiblas/flexiblas-%{version}.tgz
BuildRequires:  gcc-fortran cmake gcc gcc-c++ libatlas3-devel openblas-devel
Requires:       libopenblas0 libopenblaso0 libopenblasp0 libatlas3 libgfortran3 
BuildRequires:  update-alternatives
Requires(post): update-alternatives
Requires(preun): update-alternatives

%description
The FlexiBLAS library is a flexible BLAS wrapper
which allows one to exchange the system wide BLAS
implementation without recompiling or relinking programs.
Even dealing with runtime-loader specific tricks is
not necessary with FlexiBLAS.

You can select the BLAS implementation which is used behind
FlexiBLAS using a configuration file or a environment variable.
It is integrated in Debian/Ubuntu using the update-alternatives
mechanism.

The package supports OpenBLAS, ATLAS and the reference Netlib
implementation. If you compile FlexiBLAS on your own, other
BLAS implementations like the old GotoBLAS or the Intel MKL
are supported. 

%package -n libflexiblas 
Summary: Shared Libraries for FlexiBLAS
Group: System/Libraries

%description -n libflexiblas 
The FlexiBLAS library is a flexible BLAS wrapper
which allows one to exchange the system wide BLAS
implementation without recompiling or relinking programs.
Even dealing with runtime-loader specific tricks is
not necessary with FlexiBLAS.

You can select the BLAS implementation which is used behind
FlexiBLAS using a configuration file or a environment variable.
It is integrated in Debian/Ubuntu using the update-alternatives
mechanism.

The package supports OpenBLAS, ATLAS and the reference Netlib
implementation. If you compile FlexiBLAS on your own, other
BLAS implementations like the old GotoBLAS or the Intel MKL
are supported. 

%package devel 
Summary: Development Headers for FlexiBLAS
Group: Development/Libraries/C and C++
Requires:  lib%{name} = %{version}

%description devel 
The FlexiBLAS library is a flexible BLAS wrapper
which allows one to exchange the system wide BLAS
implementation without recompiling or relinking programs.
Even dealing with runtime-loader specific tricks is
not necessary with FlexiBLAS.

You can select the BLAS implementation which is used behind
FlexiBLAS using a configuration file or a environment variable.
It is integrated in Debian/Ubuntu using the update-alternatives
mechanism.

The package supports OpenBLAS, ATLAS and the reference Netlib
implementation. If you compile FlexiBLAS on your own, other
BLAS implementations like the old GotoBLAS or the Intel MKL
are supported. 

%prep
%setup -q

%build
mkdir build
pushd build 
cmake ../ -DCMAKE_INSTALL_PREFIX=%{_prefix} -DCMAKE_INSTALL_LIBDIR=%{_libdir} -DCMAKE_INSTALL_SYSCONFDIR=/etc \
  -DTESTS=OFF -DCBLAS=ON -DCMAKE_BUILD_TYPE=Release \
  -DOPENBLAS=OFF -DGOTO=OFF -DINTEL_32=OFF -DINTEL_32_SEQ=OFF -DINTEL_64LP=OFF -DINTEL_64LP_SEQ=OFF\
  -DAPPLE=OFF -DACML=OFF -DACML_MP=OFF -DATLAS=OFF
 
make %{?_smp_mflags}

gcc -DCBLAS_INTERFACE -Iinclude -O2 -shared -o libblas_openblas_omp.so ../src/dummy_lib.c -lopenblaso -lgomp -lm -fopenmp -fPIC -lgfortran
gcc -DCBLAS_INTERFACE -Iinclude -O2 -shared -o libblas_openblas_pthread.so ../src/dummy_lib.c -lopenblasp -lpthread -pthread -lm -fPIC -lgfortran
gcc -DCBLAS_INTERFACE -Iinclude -O2 -shared -o libblas_openblas_serial.so ../src/dummy_lib.c -lopenblas -lm  -fPIC -lgfortran 
#gcc -DSCABS_MISSING -DCBLAS_INTERFACE -Iinclude -O2 -shared -o libblas_atlas_thread.so ../src/dummy_lib.c -L%{_libdir}/atlas -lcblas -lf77blas -ltatlas -pthread -lpthread  -fPIC -lgfortran
gcc -DSCABS_MISSING -DCBLAS_INTERFACE -Iinclude -O2 -shared -o libblas_atlas_serial.so ../src/dummy_lib.c -L%{_libdir}/atlas -lf77blas -lsatlas -pthread -lpthread  -fPIC -lgfortran

echo "OpenBLAS_Serial | libblas_openblas_serial.so " >> flexiblasrc
echo "OpenBLAS_OpenMP | libblas_openblas_omp.so " >> flexiblasrc
echo "OpenBLAS_Pthread | libblas_openblas_pthread.so" >> flexiblasrc 
echo "ATLAS_Serial | libblas_atlas_serial.so" >> flexiblasrc
#echo "ATLAS_Pthread | libblas_atlas_thread.so" >> flexiblasrc 
popd 
%install

pushd build
make install DESTDIR=%{buildroot}
install -p -D -m 755 libblas_openblas_omp.so %{buildroot}%{_libdir}/flexiblas/
install -p -D -m 755 libblas_openblas_pthread.so %{buildroot}%{_libdir}/flexiblas/
install -p -D -m 755 libblas_openblas_serial.so %{buildroot}%{_libdir}/flexiblas/
install -p -D -m 755 libblas_atlas_serial.so %{buildroot}%{_libdir}/flexiblas/
#install -p -D -m 755 libblas_atlas_thread.so %{buildroot}%{_libdir}/flexiblas/

popd 
%files -n libflexiblas
%defattr(-,root,root,-)
%config  /etc/flexiblasrc
/usr/bin/flexiblas
%{_libdir}/flexiblas/libblas_netlib.so
%{_libdir}/flexiblas/libblas_openblas_omp.so
%{_libdir}/flexiblas/libblas_openblas_pthread.so
%{_libdir}/flexiblas/libblas_openblas_serial.so
%{_libdir}/flexiblas/libblas_atlas_serial.so
#%{_libdir}/flexiblas/libblas_atlas_thread.so
%{_libdir}/libflexiblas.so.0.1.0
%{_libdir}/libflexiblas.so.1

%files devel
%defattr(-,root,root,-)
/usr/include/flexiblas/cblas.h
/usr/include/flexiblas/f77blas_interface.h
%{_libdir}/libflexiblas.so
%{_libdir}/pkgconfig/flexiblas.pc

%post -n libflexiblas
"%_sbindir/update-alternatives" --install %{_libdir}/libblas.so.3 libblas.so.3 %{_libdir}/libflexiblas.so.1  60
/sbin/ldconfig

%preun -n libflexiblas 
if [ "$1" = 0 ] ; then
   "%_sbindir/update-alternatives" --remove libblas.so.3 %{_libdir}/libflexiblas.so.1
fi

%postun -n libflexiblas -p /sbin/ldconfig


%changelog
* Sun May 11 08 2014 <koehlerm@mpi-magdeburg.mpg.de>
- Version 1.1.0
* Wed Jan 08 2014 <koehlerm@mpi-magdeburg.mpg.de>
- Release 1.0.0 
* Fri Nov 15 2013 <koehlerm@mpi-magdeburg.mpg.de>
- initial RPM Package for OpenSuse 
