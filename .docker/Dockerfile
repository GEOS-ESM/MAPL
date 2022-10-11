# MAPL Dockerfile

ARG osversion
ARG imagename
ARG baselibversion
ARG mpiname
ARG mpiversion
ARG compilername
ARG compilerversion

ARG BASE_IMAGE=gmao/${osversion}-${imagename}:${baselibversion}-${mpiname}_${mpiversion}-${compilername}_${compilerversion}
FROM ${BASE_IMAGE}

ARG maplversion

RUN git clone -b ${maplversion} https://github.com/GEOS-ESM/container-test.git /MAPL-src && \
    cd /MAPL-src && \
    mepo init && mepo clone && \
    mkdir build && \
    cd build && \
    cmake .. -DCMAKE_INSTALL_PREFIX=/MAPL/install -DBASEDIR=$BASEDIR/Linux -DUSE_F2PY=OFF -DCMAKE_Fortran_COMPILER=$FC -DCMAKE_C_COMPILER=$CC -DCMAKE_CXX_COMPILER=$CXX && \
    make -j install/strip && \
    cd / && \
    rm -rf /MAPL-src
