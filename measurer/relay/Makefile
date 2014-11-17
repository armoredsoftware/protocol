build: build-relay build-fakeattester

build-relay: 
	gcc relay.c -o relay ../code/experiments/vchanTestCentOS_1/common/common.c -lxenvchan -lxenctrl -I../code/experiments/vchanTestCentOS_1/include/

build-fakeattester:
	gcc fakeattester.c -o fakeattester ../code/experiments/vchanTestCentOS_1/common/common.c -lxenvchan -lxenctrl -I../code/experiments/vchanTestCentOS_1/include/

run-relay:
	./relay 1 $(ATTESTER_DOMID)

run-fakeattester:
	./fakeattester 0 $(RELAY_ID)

clean:
	rm -rf *o relay fakeattester