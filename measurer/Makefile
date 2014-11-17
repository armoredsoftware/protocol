all: build-hotspot

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