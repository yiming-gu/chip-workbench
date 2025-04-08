BUILD_DIR = ./build

PRJ = groom

test:
	mill -i $(PRJ).test

verilog:
#	$(call git_commit, "generate verilog")
	mkdir -p $(BUILD_DIR)
	mill -i $(PRJ).runMain Elaborate --target-dir $(BUILD_DIR)

help:
	mill -i $(PRJ).runMain Elaborate --help

reformat:
	mill -i __.reformat

checkformat:
	mill -i __.checkFormat

bsp:
	mill -i mill.bsp.BSP/install

idea:
	mill -i mill.idea.GenIdea/idea

clean:
	-rm -rf $(BUILD_DIR)

# .PHONY: test verilog help reformat checkformat clean

# run:

MAX_CPU                 := $(shell nproc)
SHELL                   := $(shell which bash) -o pipefail
ABS_TOP                 := $(subst /cygdrive/c/,C:/,$(shell pwd))
SCRIPTS                 := $(ABS_TOP)/scripts
VIVADO                  ?= vivado
VIVADO_OPTS             ?= -nolog -nojournal -mode batch
FPGA_PART               ?= xc7z020clg400-1
RTL                     += $(subst /cygdrive/c/,C:/,$(shell find $(ABS_TOP)/src -type f -name "*.v"))
RTL_SV                  += $(subst /cygdrive/c/,C:/,$(shell find $(ABS_TOP)/src -type f -name "*.sv"))
CONSTRAINTS             += $(subst /cygdrive/c/,C:/,$(shell find $(ABS_TOP)/src -type f -name "*.xdc"))
TOP                     ?= $(TOPFILE)
VCS                     := vcs -full64
VCS_OPTS                := -notice -line +lint=all,noVCDE,noNS,noSVA-UA -sverilog -timescale=1ns/10ps -debug +define+ABS_TOP=$(ABS_TOP) +incdir+$(ABS_TOP)/src
## the path of verdi home
PLATFORM                := linux64
NOVAS_PATH              := ${NOVAS_HOME}/share/PLI/VCS/$(PLATFORM)
# VCS_VERDI_OPTS          := +define+FSDB -fsdb -notice -line +lint=all, -sverilog -timescale=1ns/10ps -debug -kdb -lca -P $(VERDI_HOME)/share/PLI/VCS/$(PLATFORM)/novas.tab $(VERDI_HOME)/share/PLI/VCS/$(PLATFORM)/pli.a +define+ABS_TOP=$(ABS_TOP) +incdir+$(ABS_TOP)/src
# VCS_VERDI_OPTS          := +define+FSDB -debug_access -debug_all -notice -line +lint=all, -sverilog -timescale=1ns/10ps -debug_acc+pp+f+dmptf -debug_region+cell+encrypt -kdb -lca -P $(VERDI_HOME)/share/PLI/VCS/$(PLATFORM)/novas.tab $(VERDI_HOME)/share/PLI/VCS/$(PLATFORM)/pli.a +define+ABS_TOP=$(ABS_TOP) +incdir+$(ABS_TOP)/src
VCS_VERDI_OPTS          := +define+FSDB -debug_access -debug_acc+all -debug_region+cell+encrypt -notice -line -sverilog -timescale=1ns/10ps -kdb -lca -ntb_opts uvm -P $(NOVAS_PATH)/novas.tab $(NOVAS_PATH)/pli.a +define+ABS_TOP=$(ABS_TOP) +incdir+$(ABS_TOP)/src

SIM_RTL                 := $(subst /cygdrive/c/,C:/,$(shell find $(ABS_TOP)/sim -type f -name "*.v"))
IVERILOG                := iverilog
IVERILOG_OPTS           := -Ttyp -D IVERILOG=1 -g2012 -gassertions -Wall -Wno-timescale -D ABS_TOP=$(ABS_TOP) -I $(ABS_TOP)/src -I $(ABS_TOP)/sim
VVP                     := vvp



## verilator linter
## guide: https://verilator.org/guide/latest/
## https://github.com/verilator/verilator
lint:
	@echo
	@echo "### VERILATOR LINT ###"
	verilator -DABS_TOP=$(ABS_TOP) --lint-only --top-module $(TOP) $(RTL) $(RTL_SV) -I$(ABS_TOP)/src

## vcs+dve
## $*.tb print the result of sim
sim/%.tb: sim/%.v $(RTL)
	mkdir -p sim/sim_files && cd sim/sim_files && $(VCS) $(VCS_OPTS) -o $*.tb $(RTL) ../$*.v -top $*

## dve
sim/%.vpd: sim/%.tb
	@echo
	@echo "### VCS SIMULATING ###"
	cd sim/sim_files && ./$*.tb +verbose=1 +vpdfile+$*.vpd |& tee $*.log

## iverilog
## guide: https://iverilog.fandom.com/wiki/User_Guide
sim/%.tbi: sim/%.v $(RTL)
	mkdir -p sim/sim_files && cd sim/sim_files && $(IVERILOG) $(IVERILOG_OPTS) -o $*.tbi ../$*.v $(RTL)
sim/%.fst: sim/%.tbi
	@echo
	@echo "### IVERILOG SIMULATING ###"
	cd sim/sim_files && $(VVP) $*.tbi -fst |& tee $*.log

sim/%.tbi: sim/%.sv $(RTL) $(RTL_SV)
	mkdir -p sim/sim_files && cd sim/sim_files && $(IVERILOG) $(IVERILOG_OPTS) -o $*.tbi ../$*.sv $(RTL) $(RTL_SV)
sim/%.fst: sim/%.tbi
	@echo
	@echo "### IVERILOG SIMULATING ###"
	cd sim/sim_files && $(VVP) $*.tbi -fst |& tee $*.log

## open gtkwave
## guide: https://www.renesas.com/jp/ja/document/oth/gtkwave-user-guide
%.fst: sim/%.fst
	@echo
	@echo "### OPEN GTKWAVE ###"
	cd sim/sim_files && gtkwave $*.fst


## vcs+verdi verilog
sim/%.fsdb: sim/%.v
	@echo
	@echo "### VCS SIMULATING ###"
	mkdir -p sim/sim_files && cd sim/sim_files && $(VCS) $(VCS_VERDI_OPTS) -o $*.tb $(RTL) ../$*.v -top $* && ./$*.tb
## vcs+verdi system verilog
sim/%.fsdb: sim/%.sv
	@echo
	@echo "### VCS SIMULATING ###"
	mkdir -p sim/sim_files && cd sim/sim_files && $(VCS) $(VCS_VERDI_OPTS) -o $*.tb $(RTL) $(RTL_SV) ../$*.sv -top $* && ./$*.tb

## verdi verilog
%.fsdb: sim/%.fsdb sim/%.v
	@echo
	@echo "### OPEN VERDI ###"
	mkdir -p sim/sim_files && cd sim/sim_files && verdi +v2k -sv $(RTL) ../$*.v -nologo -ssf $*.fsdb
## verdi system verilog
%.fsdb: sim/%.fsdb sim/%.sv
	@echo
	@echo "### OPEN VERDI ###"
	mkdir -p sim/sim_files && cd sim/sim_files && verdi +v2k -sv $(RTL) $(RTL_SV) ../$*.sv -nologo -ssf $*.fsdb




## generate tcl file
build/target.tcl: $(RTL) $(CONSTRAINTS)
	mkdir -p build
	truncate -s 0 $@
	echo "set ABS_TOP                        $(ABS_TOP)"    >> $@
	echo "set TOP                            $(TOP)"    >> $@
	echo "set FPGA_PART                      $(FPGA_PART)"  >> $@
	echo "set_param general.maxThreads       $(MAX_CPU)"    >> $@
	echo "set_param general.maxBackupLogs    0"    >> $@
	echo -n "set RTL { " >> $@
	FLIST="$(RTL)"; for f in $$FLIST; do echo -n "$$f " ; done >> $@
	echo "}" >> $@
	echo -n "set CONSTRAINTS { " >> $@
	FLIST="$(CONSTRAINTS)"; for f in $$FLIST; do echo -n "$$f " ; done >> $@
	echo "}" >> $@

setup: build/target.tcl

elaborate: build/target.tcl $(SCRIPTS)/elaborate.tcl
	mkdir -p ./build
	cd ./build && $(VIVADO) $(VIVADO_OPTS) -source $(SCRIPTS)/elaborate.tcl |& tee elaborate.log

build/synth/$(TOP).dcp: build/target.tcl $(SCRIPTS)/synth.tcl
	mkdir -p ./build/synth/
	cd ./build/synth/ && $(VIVADO) $(VIVADO_OPTS) -source $(SCRIPTS)/synth.tcl |& tee synth.log

synth: build/synth/$(TOP).dcp

build/impl/$(TOP).bit: build/synth/$(TOP).dcp $(SCRIPTS)/impl.tcl
	mkdir -p ./build/impl/
	cd ./build/impl && $(VIVADO) $(VIVADO_OPTS) -source $(SCRIPTS)/impl.tcl |& tee impl.log

impl: build/impl/$(TOP).bit
all: build/impl/$(TOP).bit

program: build/impl/$(TOP).bit $(SCRIPTS)/program.tcl
	cd build/impl && $(VIVADO) $(VIVADO_OPTS) -source $(SCRIPTS)/program.tcl

program-force:
	cd build/impl && $(VIVADO) $(VIVADO_OPTS) -source $(SCRIPTS)/program.tcl

vivado: build
	cd build && nohup $(VIVADO) </dev/null >/dev/null 2>&1 &

sim_build/compile_simlib/synopsys_sim.setup:
	mkdir -p sim_build/compile_simlib
	cd build/sim_build/compile_simlib && $(VIVADO) $(VIVADO_OPTS) -source $(SCRIPTS)/compile_simlib.tcl

compile_simlib: sim_build/compile_simlib/synopsys_sim.setup

screen:
	screen /dev/ttyUSB0 115200

clean-sim:
	rm -rf sim/sim_files

# rm -rf *.daidir sim/output.txt \
# sim/*.tb sim/*.daidir sim/csrc \
# sim/ucli.key sim/*.vpd sim/*.vcd \
# sim/*.tbi sim/*.fst sim/*.jou sim/*.log sim/*.out \
# ./DVEfiles \
# sim/*.fsdb \
# sim/verdiLog sim/novas.conf sim/novas.rc sim/verdi_config_file

clean-build:
	rm -rf ./build

.PHONY: setup synth impl program program-force vivado all clean-build clean-sim %.tb
.PRECIOUS: sim/%.tb sim/%.tbi sim/%.fst sim/%.vpd sim/%.fsdb
