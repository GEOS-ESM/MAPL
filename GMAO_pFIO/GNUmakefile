#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# -------------------------------------------------------------------------


# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(shell pwd)/../../..
endif

THIS := $(shell basename `pwd`)
LIB  = lib$(THIS).a

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies
  include $(ESMADIR)/Config/GMAO_base.mk

#                  ---------------------
#                  Standard ESMA Targets
#                  ---------------------

esma_help help:
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
	@echo "         SITE = $(SITE) "
	@echo "         THIS = $(THIS) "

#                  --------------------------------
#                   Recurse Make in Sub-directories
#                  --------------------------------

TARGETS = esma_install esma_clean esma_distclean esma_doc \
          install clean distclean doc 

export ESMADIR BASEDIR ARCH SITE FREAL

$(TARGETS): 
	@ t=$@; argv="$(SUBDIRS)" ;\
	  for d in $$argv; do			 \
	    ( cd $$d				;\
	      echo ""; echo Making $$t in `pwd`          ;\
	      $(MAKE) -e $$t ) \
	  done
	$(MAKE) local_$@

local_esma_install local_install: $(LIB)
	$(MKDIR) $(ESMALIB) $(ESMAINC)/$(THIS)
	$(CP) -pr *.a $(wildcard Python)   $(ESMALIB)
	$(CP) -p *.[Mm][Oo][Dd] $(IINCS)   $(ESMAINC)/$(THIS)

local_esma_clean local_clean:
	-$(RM) *~ *.[aox] *.[Mm][Oo][Dd] *___.f90

local_esma_distclean local_distclean:
	-$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd] *___.f90

local_esma_doc local_doc:
	@echo "Target $@ not implemented in `pwd`"

#                  --------------------
#                  User Defined Targets
#                  --------------------

INCS :=  NetCDF4_get_var.H  NetCDF4_put_var.H  unused_dummy.H

#CODE :=  allgather.code           bcast.code             recv.code      \
         #allgatherv.code          gather.code            scatter.code\
         #arraygather.code         sun.code               send.code\
         #arrayscatter.code        read_parallel.code     sendrecv.code\
         #allreducemin.code        allreducemax.code      allreducesum.code

SRCS :=  AbstractMessage.F90        ExtDataCollection.F90  MpiSocket.F90  ServerThread.F90 ServerThreadVector.F90 \
         AbstractRequestHandle.F90  FileMetadata.F90       NetCDF4_FileFormatter.F90  SimpleSocket.F90 \
         AbstractSocket.F90         IntegerMessageMap.F90  pFIO_Constants.F90 StringIntegerMap.F90 IntegerIntegerMap.F90 \
         AddCollectionMessage.F90   IntegerRequestMap.F90  pFIO.F90  StringVector.F90 \
         Attribute.F90              pfio_server_demo.F90   TerminateMessage.F90 \
         ClientThread.F90           IntegerSocketMap.F90   pFIO_Utilities.F90 Throw.F90 \
         CollectionIdMessage.F90    KeywordEnforcer.F90    ProtocolParser.F90 Variable.F90 CoordinateVariable.F90 \
         AbstractDataReference.F90  ArrayReference.F90     MemReference.F90 ShmemReference.F90   \
         MessageQueue.F90           RequestDataMessage.F90  WaitRequestDataMessage.F90 \
         CollectiveRequestDataMessage.F90 AbstractDirectoryService.F90 DirectoryService.F90 SimpleDirectoryService.F90 \
         MessageVisitor.F90         BaseThread.F90  RequestIdMessage.F90 DummyMessage.F90 \
         DoneMessage.F90            MpiMutex.F90    AbstractServer.F90 MpiServer.F90 OpenMPServer.F90 SimpleServer.F90 \
         AbstractSocketVector.F90  

FOPT += $(OMPFLAG)

#TESTS = utCFIO_Bundle.x utCFIO_Array.x utCFIO_Nbits.x 
#IINCS = $(wildcard MAPL_*.h) unused_dummy.H

OBJS := $(addsuffix .o, $(basename $(SRCS)))
DEPS := $(addsuffix .d, $(basename $(SRCS)))

INC_DIRS = . $(INC_ESMF) $(INC_MPI) $(INC_SDF) $(INC_gFTL)
MOD_DIRS = $(INC_DIRS) $(INC_CFIO)

USER_FINCS = $(foreach dir,$(INC_DIRS),$(I)$(dir))
USER_FMODS = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 
USER_FDEFS = $(D)MAPL $(D)DO_COMMAS $(D)TWO_SIDED_COMM

FREAL = $(FREAL4) # for now, require 32 bit reals (R4)

# MAT Apparently, if you compile MAPL_Generic with optimization -O2, it will
#     cause a SegFault. Until this is figured out, we compile it at -O0.
#     Note, for some reason this also affects GenericCplComp, Profiler, and Sun.
#     Not sure why.
ifneq ("$(BOPT)","g")
ifeq ($(ESMA_FC),gfortran)
MAPL_Generic.o: FOPT := $(FOPT0)
endif
endif

vpath % $(MOD_DIRS) tests TeX

$(LIB) lib : $(DEPS) $(OBJS)
	$(AR) $(AR_FLAGS) $(LIB) $(OBJS)
	$(RANLIB) $(RANLIB_FLAGS) $(LIB)

%.x : $(LIB) %.o
	$(FC) $(LDFLAGS) -o $@ $*.o $(LIB) \
              $(LIB_ESMF) $(LIB_CFIO) \
              $(wildcard $(LIB_MFHDF3)) $(LIB_SDF)\
              $(LIB_MPI) $(LIB_SYS)

#test: $(TESTS)
	  #@ argv="$(TESTS)" ;\
	  #for test in $$argv; do			 \
	    #( echo " "; echo "---- Starting test $$test"; \
              #mpirun -np 4 $$test ) \
	  #done

# #                  --------------------
# #                     Documentation
# #                  --------------------

# PDF_DOCS = MAPL_UsersGuide.pdf  \
#            MAPL_Base.pdf       \
#            MAPL_Cap.pdf        \
#            MAPL_CFIO.pdf       \
#            MAPL_Exceptions.pdf \
#            MAPL_HistoryGridComp.pdf       

# esma_doc doc: $(PDF_DOCS)

# MAPL_UsersGuide.tex: $(UG_SRCS)
# 	$(PROTEX) -g -b -f $(UG_SRCS) > MAPL_UsersGuide___.tex
# 	$(CAT) TeX/MAPL_Intro.tex MAPL_UsersGuide___.tex > MAPL_UsersGuide.tex

# Hack to prevent remaking dep files during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
