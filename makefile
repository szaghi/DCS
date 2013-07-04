#!/usr/bin/make
#----------------------------------------------------------------------------------------------------------------------------------
# make init

# shell
SHELL = /bin/bash
# no verbose
$(VERBOSE).SILENT:
#----------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------
# User options
COMPILER  = intel
DEBUG     = yes
F03STD    = yes
OPTIMIZE  = no
NULi      = no
NULj      = no
NULk      = no

.PHONY : DEFAULTRULE
DEFAULTRULE: $(DEXE)DCS

.PHONY : help
help:
	@echo
	@echo -e '\033[1;31m Make options of DCS code\033[0m'
	@echo
	@echo -e '\033[1;31m Compiler choice: COMPILER=$(COMPILER)\033[0m\033[1m => default\033[0m'
	@echo -e '\033[1;31m  COMPILER=gnu  \033[0m\033[1m => GNU gfortran          \033[0m'
	@echo -e '\033[1;31m  COMPILER=intel\033[0m\033[1m => Intel Fortran         \033[0m'
	@echo
	@echo -e '\033[1;31m Compiling options\033[0m'
	@echo -e '\033[1;31m  DEBUG=yes(no)    \033[0m\033[1m => on(off) debug                  (default $(DEBUG))\033[0m'
	@echo -e '\033[1;31m  F03STD=yes(no)   \033[0m\033[1m => on(off) check standard fortran (default $(F03STD))\033[0m'
	@echo -e '\033[1;31m  OPTIMIZE=yes(no) \033[0m\033[1m => on(off) optimization           (default $(OPTIMIZE))\033[0m'
	@echo
	@echo -e '\033[1;31m Preprocessing options\033[0m'
	@echo -e '\033[1;31m  NULi=yes(no) \033[0m\033[1m => on(off) nullify i direction (1D or 2D) (default $(NULi))\033[0m'
	@echo -e '\033[1;31m  NULj=yes(no) \033[0m\033[1m => on(off) nullify j direction (1D or 2D) (default $(NULj))\033[0m'
	@echo -e '\033[1;31m  NULk=yes(no) \033[0m\033[1m => on(off) nullify k direction (1D or 2D) (default $(NULk))\033[0m'
	@echo
	@echo -e '\033[1;31m Provided Rules: default=DCS\033[0m\033[1m => compile the code\033[0m'
	@echo -e '\033[1;31m  cleanobj     =>\033[0m\033[1m cleaning compiled object\033[0m'
	@echo -e '\033[1;31m  cleanmod     =>\033[0m\033[1m cleaning .mod files\033[0m'
	@echo -e '\033[1;31m  cleanmsg     =>\033[0m\033[1m cleaning make-log massage files\033[0m'
	@echo -e '\033[1;31m  clean        =>\033[0m\033[1m running cleanobj, cleanmod and cleanmsg\033[0m'
	@echo -e '\033[1;31m  cleanall     =>\033[0m\033[1m running clean and cleanexe\033[0m'
	@echo -e '\033[1;31m  tar          =>\033[0m\033[1m creating a tar archive of the project\033[0m'
	@echo -e '\033[1;31m  doc          =>\033[0m\033[1m building the documentation\033[0m'
#----------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------
# directory & file
DSRC  = ./src/
DOBJ  = ./obj/
DMOD  = ./mod/
DEXE  = ./
VPATH = $(DSRC) $(DOBJ) $(DMOD)

MKDIRS = $(DOBJ) $(DMOD) $(DEXE)
#-----------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------
# compiler specific rules
# GNU
WRN_GNU = -fmax-errors=0 -Wall -Wno-array-temporaries -Warray-bounds -Wcharacter-truncation -Wline-truncation -Wconversion-extra -Wimplicit-interface -Wimplicit-procedure -Wunderflow -Wextra -Wuninitialized
CHK_GNU = -fcheck=all
DEB_GNU = -fmodule-private -ffree-line-length-132 -fimplicit-none -ffpe-trap=invalid,overflow -fbacktrace -fdump-core -finit-real=nan #-fno-range-check  ,precision,denormal,underflow
STD_GNU = -std=f2003 -fall-intrinsics
OPT_GNU = -O3
# Intel
WRN_INT = -warn all
CHK_INT = -check all
DEB_INT = -debug all -extend-source 132 -fpe-all=0 -fp-stack-check -fstack-protector-all -ftrapuv -no-ftz -traceback -gen-interfaces
STD_INT = -std03
OPT_INT = -O3 -ipo -inline all -ipo-jobs4 -vec-report1
# setting rules according user options
ifeq "$(COMPILER)" "gnu"
  FC = gfortran
  OPTSC = -cpp -c -J$(DMOD) -static -fprotect-parens -fno-realloc-lhs
  OPTSL =
  WRN = $(WRN_GNU)
  CHK = $(CHK_GNU)
  DEB = $(DEB_GNU)
  STD = $(STD_GNU)
  OPT = $(OPT_GNU)
endif
ifeq "$(COMPILER)" "intel"
  FC = ifort
  OPTSC = -cpp -c -module $(DMOD) -static -assume protect_parens -assume norealloc_lhs -fp-model source
  OPTSL =
  WRN = $(WRN_INT)
  CHK = $(CHK_INT)
  DEB = $(DEB_INT)
  STD = $(STD_INT)
  OPT = $(OPT_INT)
endif
ifeq "$(DEBUG)" "yes"
  PREPROC := $(PREPROC) -DDEBUG
  OPTSC := $(OPTSC) -O0 -C -g $(WRN) $(CHK) $(DEB)
  OPTSL := $(OPTSL) -O0 -C -g $(WRN) $(CHK) $(DEB)
endif
ifeq "$(F03STD)" "yes"
  OPTSC := $(OPTSC) $(STD)
  OPTSL := $(OPTSL) $(STD)
endif
ifeq "$(OPTIMIZE)" "yes"
  OPTSC := $(OPTSC) $(OPT)
  OPTSL := $(OPTSL) $(OPT)
endif
# pre-processing options
# 1D or 2D solver
NULiCHK = (Unknown NULi switch) Used default NULi=no
ifeq "$(NULi)" "no"
  NULiCHK = (Known NULi switch) Used NULi=$(NULi)
endif
ifeq "$(NULi)" "yes"
  NULiCHK = (Known NULi switch) Used NULi=$(NULi)
  PREPROC := $(PREPROC) -DNULi
endif
NULjCHK = (Unknown NULj switch) Used default NULj=no
ifeq "$(NULj)" "no"
  NULjCHK = (Known NULj switch) Used NULj=$(NULj)
endif
ifeq "$(NULj)" "yes"
  NULjCHK = (Known NULj switch) Used NULj=$(NULj)
  PREPROC := $(PREPROC) -DNULj
endif
NULkCHK = (Unknown NULk switch) Used default NULk=no
ifeq "$(NULk)" "no"
  NULkCHK = (Known NULk switch) Used NULk=$(NULk)
endif
ifeq "$(NULk)" "yes"
  NULkCHK = (Known NULk switch) Used NULk=$(NULk)
  PREPROC := $(PREPROC) -DNULk
endif
OPTSC := $(OPTSC) $(PREPROC)
OPTSL := $(OPTSL) $(PREPROC)

WHICHFC = $(shell which $(FC))

PRINTCHK = "\\033[1;31m Compiler used \\033[0m\\033[1m $(COMPILER) => $(WHICHFC)\\033[0m \n\
            \\033[1;31mSource dir    \\033[0m\\033[1m $(DSRC)\\033[0m \n\
            \\033[1;31m Debug         \\033[0m\\033[1m $(DEBUG)\\033[0m \n\
            \\033[1;31m F-standard    \\033[0m\\033[1m $(F03STD)\\033[0m \n\
            \\033[1;31m Optimize      \\033[0m\\033[1m $(OPTIMIZE)\\033[0m \n\
            \\033[1;31m Nullify i     \\033[0m\\033[1m $(NULiCHK)\\033[0m \n\
            \\033[1;31m Nullify j     \\033[0m\\033[1m $(NULjCHK)\\033[0m \n\
            \\033[1;31m Nullify k     \\033[0m\\033[1m $(NULkCHK)\\033[0m"
#-----------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------
# auxiliary rules
.PHONY : PRINTINFO
.NOTPARALLEL : PRINTINFO
PRINTINFO:
	@echo | tee make.log
	@echo -e $(PRINTCHK) | tee -a make.log
	@echo | tee -a make.log
	@echo -e "\033[1;31m Compiling options\033[0m" | tee -a make.log
	@echo -e "\033[1m [$(OPTSC)]\033[0m" | tee -a make.log
	@echo | tee -a make.log
	@echo -e "\033[1;31m Linking options \033[0m" | tee -a make.log
	@echo -e "\033[1m [$(OPTSL)]\033[0m" | tee -a make.log
	@echo | tee -a make.log

.PHONY : $(MKDIRS)
$(MKDIRS):
	@mkdir -p $@

.PHONY : cleanobj
cleanobj:
	@echo -e "\033[1;31m deleting objects \033[0m" | tee make.log
	@rm -fr $(DOBJ)

.PHONY : cleanmod
cleanmod:
	@echo -e "\033[1;31m deleting mods \033[0m" | tee -a make.log
	@rm -fr $(DMOD)

.PHONY : cleanexe
cleanexe:
	@echo -e "\033[1;31m deleting exes \033[0m" | tee -a make.log
	@rm -f $(addprefix $(DEXE),DCS)

.PHONY : cleanmsg
cleanmsg:
	@rm -f diagnostic_messages
	@rm -f error_messages

.PHONY : clean
clean: cleanobj cleanmod cleanmsg

.PHONY : cleanall
cleanall: clean cleanexe

.PHONY : tar
tar: cleanall
	@echo -e "\033[1;31m Creating tar archive of the code \033[0m" | tee make.log
	@rm -f Driven_Cavity
	@mkdir -p Driven_Cavity
	@cp -rL src makefile Driven_Cavity/
	@tar czf Driven_Cavity.tgz Driven_Cavity
	@rm -rf Driven_Cavity

.PHONY : doc
doc:
	@echo -e "\033[1;31m Building documentation\033[0m" | tee make.log
	@doxygen .doxygenconfig
#-----------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------
# rules of linking and compiling
COTEXT  = -e "\033[1;31m Compiling\033[0m\033[1m $(<F)\033[0m"
LITEXT  = -e "\033[1;31m Assembling\033[0m\033[1m $@\033[0m"

$(DEXE)DCS : PRINTINFO $(MKDIRS) $(DOBJ)dcs.o
	@echo | tee -a make.log
	@echo $(LITEXT) | tee -a make.log
	@$(FC) $(OPTSL) $(DOBJ)*.o $(LIBS) -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)ir_precision.o : IR_Precision.f90
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)dcs.o : DCS.f90 \
	$(DOBJ)ir_precision.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages
#-----------------------------------------------------------------------------------------------------------------------------------
