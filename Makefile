
all: build

build:
	make -C cmd

clean:
	make -C cmd clean

lint:
	golangci-lint run

testlint: lint
	make -C test

test:
	make -C test

.PHONY: all lint clean build test test
