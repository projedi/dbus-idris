.PHONY: build check default install tests

default: build

check:
	idris --checkpkg dbus.ipkg

build:
	idris --build dbus.ipkg

install:
	idris --install dbus.ipkg

test: install
	idris --testpkg dbus-tests.ipkg
