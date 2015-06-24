current_dir = $(shell pwd)

all: build-measurer build-test build-driver

configure-measurer:
	cd gdb-7.9; make clean; ./configure
	cp gdb-7.9/patch/Makefile gdb-7.9/gdb/

build-measurer:
	cd gdb-7.9; make

build-driver:
	cd driver; make

build-test:
	cd test; make

run-measurer:
	./gdb-7.9/gdb/gdb

run-driver:
	cd driver; make run-driver1

run-test:
	cd test; make run-test2

run-all:
	gnome-terminal --hide-menubar --title="Measurer" -x bash -c "make run-measurer; echo \"JOBS DONE.\"; read"
	gnome-terminal --hide-menubar --title="Application" -x bash -c "make run-test; echo \"JOBS DONE.\"; read"
	gnome-terminal --hide-menubar --title="Driver" -x bash -c "echo \"pidof test2\"; pidof test2; make run-driver; echo \"JOBS DONE.\"; read"

clean: clean-measurer clean-driver clean-test

clean-measurer:
	cd gdb-7.9; make clean

clean-driver:
	cd driver; make clean

clean-test:
	cd test; make clean
