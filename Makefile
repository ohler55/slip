ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

all: build

build:
	go mod tidy
	make -C cmd

clean:
	make -C cmd clean

lint:
	golangci-lint run

fetch-plugins:
	git clone git@github.com:ohler55/slip-flow.git plugins/slip-flow
	git clone git@github.com:ohler55/slip-ggql.git plugins/slip-ggql
	git clone git@github.com:ohler55/slip-jet.git plugins/slip-jet
	git clone git@github.com:ohler55/slip-message.git plugins/slip-message
	git clone git@github.com:ohler55/slip-mongo.git plugins/slip-mongo
	git clone git@github.com:ohler55/slip-parquet.git plugins/slip-parquet

clean-plugins:
	rm -rf plugins/slip-flow
	rm -rf plugins/slip-ggql
	rm -rf plugins/slip-jet
	rm -rf plugins/slip-message
	rm -rf plugins/slip-mongo
	rm -rf plugins/slip-parquet

plugins:
	$(foreach plugin, $(wildcard ./plugins/slip-*), cd $(plugin); go mod edit --replace github.com/ohler55/slip=../..; make ; go mod edit --replace github.com/ohler55/slip=../slip; cd $(ROOT_DIR);)

testlint: lint
	make -C test

test:
	make -C test cover

test-plugins: plugins
	go run cmd/slip/main.go -e '(terpri)' plugins/require-test.lisp

.PHONY: all lint clean build test test plugins
