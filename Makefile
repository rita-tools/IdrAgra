# version string for the header: use an exact tag when present, otherwise the commit string
VERSION := $(shell git describe --tags --exact-match HEAD 2>/dev/null || git rev-parse --short HEAD)
CURRENTDATE := $(shell date -u +%Y-%m-%dT%H:%M:%SZ)
LOG_FILE := $(shell git log --format=reference > ./release/git_idragra.log)

# OS variables & settings
DEL = rm
CP = cp

ifeq ($(OS),Windows_NT)
   EXE = .exe
   WIN = 1
   STATIC_FLAG = -static
else
   EXE =
   WIN = 0
   STATIC_FLAG =
endif

# Compiler settings
# -cpp: activates compiler pre processing
# -DGIT_VERSION: sets the macro GIT_VERSION in the code (actually used only in cli_main.f90)
# -g: enables debug with breakpoints 

ifeq ($(OS),Windows_NT)
   MINGW64_BINDIR ?= /mingw64/bin
   export PATH := $(MINGW64_BINDIR):$(PATH)
endif

CC = gfortran
CPP = gfortran -cpp

# Build mode:
#   make         -> release build
#   make debug   -> debug build
#   make debug=1 -> debug build
debug ?= 0

ifeq ($(debug),1)
   TYPE := debug
   GFFLAGS = -cpp -DGIT_VERSION=\"$(VERSION)\" -DCOMP_DATE=\"$(CURRENTDATE)\" -DWIN=$(WIN) -g -Wall -Wconversion -fimplicit-none -fbacktrace -ffree-line-length-0 -fcheck=all,no-array-temps -ffpe-trap=zero,overflow,underflow -finit-real=nan -c
   LDFLAGS = -g
else
   TYPE := release
   GFFLAGS = -cpp -DGIT_VERSION=\"$(VERSION)\" -DCOMP_DATE=\"$(CURRENTDATE)\" -DWIN=$(WIN) -ffast-math -O3 -ffree-line-length-0 -c
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

FILES = mod_constants mod_utility mod_parameters mod_grid mod_common mod_evapotranspiration mod_meteo \
		mod_crop_phenology mod_crop_soil_water mod_runoff mod_TDx_index mod_irrigation mod_system \
		cli_watsources cli_crop_parameters cli_save_outputs cli_read_parameter cli_simulation_manager

#### User, don't touch the following line ####

# Builds the app
# force removing main.o in order to update program metadata
$(APPNAME): $(patsubst %, $(OBJDIR)/%.o, $(FILES))
	$(DEL) -f $(OBJDIR)/cli_main.o
	$(CC) -o $(OBJDIR)/cli_main.o -J$(OBJDIR) $(GFFLAGS) $(SRCDIR)/cli_main.f90
	$(CPP) -o $(RELDIR)/$@$(EXE) $^ $(OBJDIR)/cli_main.o $(LDFLAGS) $(STATIC_FLAG)
	$(CP) $(RELDIR)/$@$(EXE) $(RELDIR)/$(APPALIAS)$(EXE)

# The following sets a function that creates makefile rules
# Additionally:
# -o: set path to output
# -J: set path for *.mod
define make-o-rule
$(OBJDIR)/$1.o: $(SRCDIR)/$1.f90
	$(CC) -o $(OBJDIR)/$1.o -J$(OBJDIR) $(GFFLAGS) $(SRCDIR)/$1.f90
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
