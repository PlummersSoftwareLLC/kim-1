	
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

all: kimDemo.hex

kimDemo.bin: kimDemo.c 
	$(CL) -t kim1 --listing kimDemo.lst -C kim1-mtu60k.cfg -o kimDemo.bin kimDemo.c ../mtuGFX/lib/mtuGFX.lib -Ln kimgfx.lbl

kimDemo.hex: kimDemo.bin 
	srec_cat kimDemo.bin -binary -offset 0x2000 -o kimDemo.hex  -Intel -address-length=2

kimDEMO.ptp: kimDemo.bin
	srec_cat kimDemo.bin -binary -offset 0x2000 -o kimDemo.ptp -MOS_Technologies

clean:
	@$(DEL) *.bin *.lst *.lbl *.ptp *.hex 2>$(NULLDEV)
  

