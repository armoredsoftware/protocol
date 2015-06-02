current_dir = $(shell pwd)

all: build-measurer build-test build-driver

configure-measurer:
	cd gdb-7.9; make clean; ./configure
	cp gdb-7.9/patch/Makefile gdb-7.9/gdb/

build-measurer:
	cd gdb-7.9; make

test2:
	cd gdb-7.9; pwd > test

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
