
all: build

clean:
	rm *.so

build:
	go build -buildmode=plugin -o testplugin.so *.go

.PHONY: all build
