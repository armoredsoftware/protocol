current_dir = $(shell pwd)

all: build-hotspot

preparejava:
	export JAVA_HOME=$(current_dir)/jdk1.6.0_45; echo $$JAVA_HOME
	export PATH=$(current_dir)/jdk1.6.0_45/bin:$$PATH
	export ALT_BOOTDIR=$(current_dir)/jdk1.6.0_45
	export LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:$(current_dir)/hotspot/build/linux/linux_i486_compiler1/jvmg
	echo $$JAVA_HOME

preparejava2:
	source javasetup


build-hotspot: 
	cd hotspot/make; make jvmg1

build-relay:
	cd relay; make relay

build-fakeattester:
	cd relay; make fakeattester

run-relay:
	cd relay; make run-relay ATTESTER_DOMID=$(ATTESTER_DOMID)

run-fakeattester:
	cd relay; make run-fakeattester

clean:
	cd hotspot/make; make clean