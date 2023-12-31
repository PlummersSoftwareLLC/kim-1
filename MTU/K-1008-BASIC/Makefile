# Builds MSBASIC at x2000-x426F
# Builds K-1008 extensions at x4270-

# Run 'make SYS=<target>'; or, set a SYS env.
# var. to build for another target system.
SYS ?= kim1

BASIC_DIR = $(CURDIR)/msbasic

# Just the usual way to find out if we're
# using cmd.exe to execute make rules.
ifneq ($(shell echo),)
  CMD_EXE = 1
endif

ifdef CMD_EXE
  NULLDEV = nul:
  DEL = -del /f
  RMDIR = rmdir /s /q
else
  NULLDEV = /dev/null
  DEL = $(RM)
  RMDIR = $(RM) -r
endif

ifdef CC65_HOME
  AS = $(CC65_HOME)/bin/ca65
  CC = $(CC65_HOME)/bin/cc65
  CL = $(CC65_HOME)/bin/cl65
  LD = $(CC65_HOME)/bin/ld65
else
  AS := $(if $(wildcard ../../bin/ca65*),../../bin/ca65,ca65)
  CC := $(if $(wildcard ../../bin/cc65*),../../bin/cc65,cc65)
  CL := $(if $(wildcard ../../bin/cl65*),../../bin/cl65,cl65)
  LD := $(if $(wildcard ../../bin/ld65*),../../bin/ld65,ld65)
endif

EXELIST_kim1 = 	\
		kb9.hex \
		vmbas.hex

ifneq ($(EXELIST_$(SYS)),)
samples: $(EXELIST_$(SYS))
else
samples: notavailable
endif

# empty target used to skip systems that will not work with any program in this dir
notavailable:
ifeq ($(MAKELEVEL),0)
	@echo "info: kim1 tests not available for" $(SYS)
else
# suppress the "nothing to be done for 'samples' message
	@echo > $(NULLDEV)
endif

vmbas.bin: vmbas.o
	$(CL) -t kim1 -C kim1-60k.cfg -Oi -o vmbas.bin vmbas.o

vmbas.hex: vmbas.bin
	srec_cat vmbas.bin -binary -offset 0x4270 -o vmbas.hex -Intel -address-length=2

vmbas.o: vmbas.asm
	$(AS) vmbas.asm -o vmbas.o

kb9.hex: msbasic/orig/kb9.bin
	srec_cat msbasic/orig/kb9.bin -binary -offset 0x2000 -o kb9.hex -Intel -address-length=2

msbasic/orig/kb9.bin:
	cd msbasic && ./make.sh

clean:
	@$(DEL) vmbas.o		   	2>$(NULLDEV)
	@$(DEL) vmbas.bin	    	2>$(NULLDEV)
	@$(DEL) vmbas.hex    		2>$(NULLDEV)
	@$(DEL) kb9.hex      		2>$(NULLDEV)
	@$(DEL) msbasic/tmp/kb9.bin 	2>$(NULLDEV)


