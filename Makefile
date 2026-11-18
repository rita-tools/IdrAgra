# version string for the header: use an exact tag when present, otherwise the commit string
VERSION := $(shell git describe --tags --exact-match HEAD 2>/dev/null || git rev-parse --short HEAD)
CURRENTDATE := $(shell date -u +%Y-%m-%dT%H:%M:%SZ)
LOG_FILE := $(shell git log --format=reference > ./release/git_idragra.log)

# OS variables & settings
DEL = rm
CP = cp

ifeq ($(OS),Windows_NT) # Windows
   EXE = .exe
   WIN = 1
   STATIC_FLAG = -static
else
   EXE =
   WIN = 0
   ifeq ($(shell uname -s),Darwin) # macOS
      STATIC_FLAG = -static-libgfortran -static-libgcc -static-libquadmath
   else # Others (e.g. Linux)
      STATIC_FLAG =
   endif
endif

# Compiler settings
CC = gfortran
CPP = gfortran -cpp

# Build mode:
#   make         -> release build
#   make debug   -> debug build
#   make debug=1 -> debug build
debug ?= 0

# Uses CPP pre-processing to obtain GIT_VERSION, COMP_DATE and WIN, which are accessed by the fortran code
CPPFLAGS = -cpp -DGIT_VERSION=\"$(VERSION)\" -DCOMP_DATE=\"$(CURRENTDATE)\" -DWIN=$(WIN)

ifeq ($(debug),1)
   TYPE := debug
   FFLAGS = -ffree-line-length-0 -g -Wall -Wconversion -fimplicit-none -fbacktrace -fcheck=all,no-array-temps -ffpe-trap=zero,overflow,underflow -finit-real=nan
   LDFLAGS = -g
else
   TYPE := release
   FFLAGS = -ffree-line-length-0 -ffast-math -O3
   LDFLAGS =
endif

APPNAME = idragra_$(VERSION)_$(TYPE)
APPALIAS = idragra_latest
EXT = .f90
SRCDIR = src
OBJDIR = obj
RELDIR = release

# List of file names, without extention separated by space
# Check the list sequence according to compile order
# interf_bilancio 

FILES = mod_constants mod_date mod_utility mod_parameters mod_grid mod_common mod_evapotranspiration mod_meteo \
		mod_crop_phenology mod_cropcoef mod_crop_soil_water mod_runoff mod_irrigation mod_system \
		cli_watsources cli_crop_parameters mod_crop_yield cli_save_outputs cli_read_parameter cli_simulation_manager

#### User, don't touch the following line ####

# Builds the app
# force removing main.o in order to update program metadata
$(APPNAME): $(patsubst %, $(OBJDIR)/%.o, $(FILES))
	$(DEL) -f $(OBJDIR)/cli_main.o
	$(CC) -o $(OBJDIR)/cli_main.o -J$(OBJDIR) $(CPPFLAGS) $(FFLAGS) -c $(SRCDIR)/cli_main.f90
	$(CPP) -o $(RELDIR)/$@$(EXE) $^ $(OBJDIR)/cli_main.o $(LDFLAGS) $(STATIC_FLAG)
	$(CP) $(RELDIR)/$@$(EXE) $(RELDIR)/$(APPALIAS)$(EXE)

# The following sets a function that creates makefile rules
# Additionally:
# -o: set path to output
# -J: set path for *.mod
define make-o-rule
$(OBJDIR)/$1.o: $(SRCDIR)/$1.f90
	$(CC) -o $(OBJDIR)/$1.o -J$(OBJDIR) $(CPPFLAGS) $(FFLAGS) -c $(SRCDIR)/$1.f90
all: $1.o
endef

$(foreach element,$(FILES),$(eval $(call make-o-rule,$(element))))

all:
	$(APPNAME)

.PHONY: debug
debug:
	$(MAKE) cleanobjects
	$(MAKE) debug=1

##### Clean build outputs ####
.PHONY: cleanobjects
cleanobjects:
	$(DEL) -f $(wildcard ./$(OBJDIR)/*.mod)
	$(DEL) -f $(wildcard ./$(OBJDIR)/*.o)

.PHONY: cleanall
cleanall: cleanobjects
	$(DEL) -f $(wildcard ./$(RELDIR)/idragra_*)

.PHONY: cleanmain
cleanmain:
	@echo "hello from cleanmain"
	$(DEL) -f $(OBJDIR)/main.o
