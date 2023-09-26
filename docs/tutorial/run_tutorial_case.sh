#!/bin/bash -f

#######################################################################
#                     Batch Parameters for Run Job
#######################################################################

umask 022

ulimit -s unlimited

#######################################################################
#           Architecture Specific Environment Variables
#######################################################################

export site=NCCS

export INSTALL_DIR=$1
export TUTORIAL_CASE=$2
source $INSTALL_DIR/bin/g5_modules.sh
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib:${INSTALL_DIR}/lib
export TUTORIAL_PATH=${INSTALL_DIR}/etc/mapl_tutorials/${TUTORIAL_CASE}

for file in `ls ${TUTORIAL_PATH}`; do
  cp "${TUTORIAL_PATH}/${file}" .
done

ROOT_LIB=`cat root_lib`

export ROOT_RC=`grep '^\s*ROOT_CF:' CAP.rc | cut -d: -f2`
export NX=`grep '^\s*NX:' ${ROOT_RC}  | cut -d: -f2`
export NY=`grep '^\s*NY:' ${ROOT_RC}  | cut -d: -f2`

export NPES=`expr ${NY} \* ${NX}`

mpirun -np ${NPES} ${INSTALL_DIR}/bin/Example_Driver.x --root_dso ${ROOT_LIB}
