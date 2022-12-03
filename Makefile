
all: build

build:
	echo "Build something here"

clean:
	echo "Clean up builds"

lint:
	golangci-lint run

test: lint
	make -C test

.PHONY: all lint test
