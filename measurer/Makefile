current_dir = $(shell pwd)

COMMON_path = ../../../code/experiments/vchanTestCentOS_1/common/common.c
INCLUDE_path = ../../../code/experiments/vchanTestCentOS_1/include/


all: build-hotspot build-relay build-fakeattester

build-hotspot-prepjava:
	export JAVA_HOME=$(current_dir)/jdk1.6.0_45
	export PATH=$(current_dir)/jdk1.6.0_45/bin:$$PATH
	export ALT_BOOTDIR=$(current_dir)/jdk1.6.0_45
	export LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:$(current_dir)/hotspot/build/linux/linux_i486_compiler1/jvmg

build-hotspot:
	export JAVA_HOME=$(current_dir)/jdk1.6.0_45;export PATH=$(current_dir)/jdk1.6.0_45/bin:$$PATH;export ALT_BOOTDIR=$(current_dir)/jdk1.6.0_45;export LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:$(current_dir)/hotspot/build/linux/linux_i486_compiler1/jvmg;cd hotspot/make; make jvmg1

build-hotspot-noprep: 
	cd hotspot/make; make jvmg1

build-relay:
	cd relay; make build-relay

build-fakeattester:
	cd relay; make build-fakeattester 

run-hotspot:
	/root/Research/hotspot/build/linux/linux_i486_compiler1/jvmg/gamma -jar $(APP)

run-hotspot-hackableloop:
	/root/Research/hotspot/build/linux/linux_i486_compiler1/jvmg/gamma -jar applications/loop-hackable/ControlVar.jar

run-relay:
	cd relay; make run-relay DOMID=$(DOMID)

run-fakeattester:
	cd relay; make run-fakeattester DOMID=$(DOMID)

clean:
	cd hotspot/make; make clean