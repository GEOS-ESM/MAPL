#! /bin/make

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(shell pwd)/../../../
endif

THIS := $(shell basename `pwd`)

BINS := utCFIO_Bundle.x

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies
  include $(ESMADIR)/Config/GMAO_base.mk

TARGETS = esma_install esma_clean esma_distclean esma_doc \
          install clean distclean doc tests

export ESMADIR BASEDIR ARCH SITE FREAL

esma_install install: $(BINS)

esma_clean clean:
	-$(RM) *~ *.[aox] *.[Mm][Oo][Dd] *___.f90

esma_distclean distclean:
	-$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd] *___.f90

esma_doc doc:
	@echo "Target $@ not implemented in `pwd`"


.SUFFIXES: .F90


PF_TESTS = test_sort.pf test_horzTransform.pf # Test_CFIO_Bundle.pf
PF_SRCS = $(addsuffix .F90, $(basename $(PF_TESTS))) Test_SimpleMAPLcomp.F90 Test_CFIO_Bundle.F90
PF_OBJS := $(addsuffix .o, $(basename $(PF_SRCS)))

SRCS := testhorz.F90 utCFIO_Array.F90 utCFIO_Bundle.F90 utCFIO_Nbits.F90 utDistIO.F90 ut_ExtData.F90 \
        ESMF_TestCase.F90 ESMF_TestMethod.F90 MAPL_Initialize.F90 \
        $(addsuffix .F90, $(basename $(PF_SRCS)))

OBJS := $(addsuffix .o, $(basename $(SRCS)))
DEPS := $(addsuffix .d, $(basename $(SRCS)))

INC_DIRS = . $(INC_MAPL_BASE) $(INC_MAPL_PFUNIT) $(INC_ESMF) $(INC_SDF) $(INC_CFIO) $(INC_MPEU) $(INC_MPI)
MOD_DIRS = . $(INC_DIRS)

INC_DIRS += $(INC_PFUNIT)
MOD_DIRS += $(MOD_PFUNIT)

USER_FINCS  = $(foreach dir,$(INC_DIRS),$(I)$(dir))
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir))
         
FREAL = $(FREAL4)
THIS_CFIO = MAPL_cfio_r4

#                  --------------------
#                      Dependencies
#                  --------------------

vpath % $(MOD_DIRS)

tests: tests.x
	mpirun -np 4 ./tests.x

tests.x: $(INC_PFUNIT)/driver.F90 $(PF_OBJS)
	$(FC) $(LDFLAGS) -o $@ $+ \
              -DUSE_MPI=YES -DPFUNIT_EXTRA_INITIALIZE=MAPL_Initialize $(M)$(MOD_PFUNIT) \
	      $(I)$(INC_MAPL_PFUNIT) $(LIB_MAPL_PFUNIT) \
              $(LIB_MAPL_BASE) \
              $(LIB_ESMF) $(LIB_CFIO) $(LIB_MPEU) \
              $(wildcard $(LIB_MFHDF3)) $(LIB_SDF)  $(LIB_PFUNIT) \
              $(LIB_MPI) $(LIB_SYS)

utCFIO_Array.x : utCFIO_Array.o
	$(FC) $(LDFLAGS) -o $@ $+ $(LIB_MAPL_BASE) \
              $(LIB_ESMF) $(LIB_CFIO) $(LIB_MPEU) \
              $(wildcard $(LIB_MFHDF3)) $(LIB_SDF)\
              $(LIB_MPI) $(LIB_SYS)

utCFIO_Bundle.x : utCFIO_Bundle.o
	$(FC) $(LDFLAGS) -o $@ $+ $(LIB_MAPL_BASE) \
              $(LIB_ESMF) $(LIB_CFIO) $(LIB_MPEU) \
              $(wildcard $(LIB_MFHDF3)) $(LIB_SDF)\
              $(LIB_MPI) $(LIB_SYS)

utCFIO_Nbits.x : utCFIO_Nbits.o
	$(FC) $(LDFLAGS) -o $@ $+ $(LIB_MAPL_BASE) \
              $(LIB_ESMF) $(LIB_CFIO) $(LIB_MPEU) \
              $(wildcard $(LIB_MFHDF3)) $(LIB_SDF)\
              $(LIB_MPI) $(LIB_SYS)


  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
