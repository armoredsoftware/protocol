COMMON_path = ../../../code/experiments/vchanTestCentOS_1/common/common.c
INCLUDE_path = ../../../code/experiments/vchanTestCentOS_1/include/

build: build-relay build-fakeattester

build-relay: 
	gcc relay.c -o relay $(COMMON_path) -lxenvchan -lxenctrl -I$(INCLUDE_path)

build-fakeattester:
	gcc fakeattester.c -o fakeattester $(COMMON_path) -lxenvchan -lxenctrl -I$(INCLUDE_path)

run-relay:
	./relay 1 $(ATTESTER_DOMID)

run-fakeattester:
	./fakeattester 0 $(RELAY_DOMID)

clean:
	rm -rf *o relay fakeattester