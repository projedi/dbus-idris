.PHONY: build check default install test

default: build

check:
	idris --checkpkg dbus.ipkg

build:
	idris --build dbus.ipkg

install:
	idris --install dbus.ipkg

test: build
	idris --testpkg dbus-tests.ipkg
