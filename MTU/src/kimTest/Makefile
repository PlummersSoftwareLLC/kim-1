	
# Run 'make SYS=<target>'; or, set a SYS env.
# var. to build for another target system.
SYS ?= kim1

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

all: kimSieve.bin

kimSieve.bin:
	$(CL) -t kim1 --listing kimTest.lst -C kim1-mtu60k.cfg -o kimTest.bin kimTest.c -Ln kimTest.lbl

clean:
	@$(DEL) *.bin *.lst *.lbl  2>$(NULLDEV)



