#
# recursive makefile for ESMA.
#

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(PWD)/../..
endif


# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies

#                  ---------------------
#                  Standard ESMA Targets
#                  ---------------------

esma_help :
	@echo "Standard ESMA targets:"
	@echo "% make esma_install    (builds and install under ESMADIR)"
	@echo "% make esma_clean      (removes deliverables: *.[aox], etc)"
	@echo "% make esma_distclean  (leaves in the same state as cvs co)"
	@echo "% make esma_doc        (generates PDF, installs under ESMADIR)"
	@echo "% make esma_help       (this message)"
	@echo "Environment:"
	@echo "      ESMADIR = $(ESMADIR)"
	@echo "      BASEDIR = $(BASEDIR)"
	@echo "         ARCH = $(ARCH)"
	@echo "         SITE = $(SITE)"

THIS = $(shell basename `pwd`)
LIB = lib$(THIS).a

esma_install install: $(DEPS) $(LIB)
	$(MKDIR) $(ESMALIB) $(ESMAETC) $(ESMAINC)/$(THIS)
	$(CP) -p *.a    $(ESMALIB)
	$(CP) -p *.mod  $(ESMAINC)/$(THIS)
###	$(CP) -p *.rc   $(ESMAETC)

esma_clean clean:
	$(RM) -f *~ *.[aox] *.[Mm][Oo][Dd]

esma_distclean distclean:
	$(RM) -f *~ *.[aoxd] *.[Mm][Oo][Dd]

esma_doc doc:
	@echo "Target $@ not implemented yet in `pwd`"


TARGETS = esma_install esma_clean esma_distclean esma_doc \
          help install doc clean distclean tests

export ESMADIR BASEDIR ARCH SITE

SRCS = ESMF_TestCase.F90 ESMF_TestMethod.F90 MAPL_Initialize.F90
OBJS = $(SRCS:.F90=.o)

INC_DIRS = .
MOD_DIRS = . $(PFUNIT)/mod

USER_FINCS  = $(foreach dir,$(INC_DIRS),$(I)$(dir))
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir))

$(LIB) tests : $(DEPS) $(OBJS)
	$(AR) $(AR_FLAGS) $(LIB) $(OBJS)
	$(RANLIB) $(RANLIB_FLAGS) $(LIB)

local_esma_install local_install: $(LIB)
	@echo No local install in here


  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
