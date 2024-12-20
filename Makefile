ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

all: build

build:
	make -C cmd

clean:
	make -C cmd clean

lint:
	golangci-lint run

plugins:
	$(foreach plugin, $(wildcard ./plugins/*), cd $(plugin); go mod edit --replace github.com/ohler55/slip=../..; make ;cd $(ROOT_DIR);)

testlint: lint
	make -C test

test:
	make -C test

.PHONY: all lint clean build test test plugins 
