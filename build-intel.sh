#!/bin/bash

module purge
module use /nopt/nrel/apps/modules/candidate/modulefiles
module load comp-intel/2017.0.2

EXTRA_ARGS="$@"

CC=icc CXX=icpc FC=ifort cmake \
  -DCMAKE_BUILD_TYPE=RELEASE \
  -DCMAKE_Fortran_FLAGS="-g -fpic -simd -qopt-report=5 -align records" \
  $EXTRA_ARGS ../src

make -j 8
