current_dir = $(shell pwd)

all: build-measurer build-test build-driver

build-measurer:
	cd gdb-7.9; make

build-driver:
	cd driver; make

build-test:
	cd test; make

run-driver:
	cd driver; make run-driver1

clean: clean-measurer clean-driver clean-test

clean-measurer:
	cd gdb-7.9; make clean

clean-driver:
	cd driver; make clean

clean-test:
	cd test; make clean
