
all: build

build:
	make -C slip

clean:
	make -C slip

lint:
	golangci-lint run

test: lint
	make -C slip

.PHONY: all lint clean build test
