COMMON_path = ../../../code/experiments/vchanTestCentOS_1/common/common.c
INCLUDE_path = ../../../code/experiments/vchanTestCentOS_1/include/

build: build-relay build-fakeattester

build-relay: 
	gcc relay.c -o relay $(COMMON_path) -lxenvchan -lxenctrl -I$(INCLUDE_path)

build-fakeattester:
	gcc fakeattester.c -o fakeattester $(COMMON_path) -lxenvchan -lxenctrl -I$(INCLUDE_path)

run-relay:
	modprobe xen_gntalloc
	modprobe xen_evtchn
	modprobe xen_gntdev
	./relay 1 $(DOMID)

run-fakeattester:
	modprobe xen_gntalloc
	modprobe xen_evtchn
	modprobe xen_gntdev
	./fakeattester 0 $(DOMID)

clean:
	rm -rf *o relay fakeattester