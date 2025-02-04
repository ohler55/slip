ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

all: build

build:
	go mod tidy
	make -C cmd

clean:
	make -C cmd clean

lint:
	golangci-lint run

plugins:
	$(foreach plugin, $(wildcard ./plugins/*), cd $(plugin); go mod edit --replace github.com/ohler55/slip=../..; make ; go mod edit --replace github.com/ohler55/slip=../slip; cd $(ROOT_DIR);)

testlint: lint
	make -C test

test:
	make -C test cover

.PHONY: all lint clean build test test plugins
