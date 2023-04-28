
all: cover

cover:
	make -C testplugin build
	go test -coverpkg github.com/ohler55/slip/pkg/cl -coverprofile=cov.out

.PHONY: all cover
