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

for file in `ls ${TUTORIAL_CASE}`; do
  cp "${TUTORIAL_CASE}/${file}" .
done

ROOT_LIB=`cat root_lib`

mpirun -np 1 ${INSTALL_DIR}/bin/Example_Driver.x --root_dso ${ROOT_LIB}
