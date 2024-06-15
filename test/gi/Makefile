
all: cover

cover:
	make -C ../cl/testplugin build
	go test -coverpkg github.com/ohler55/slip/pkg/gi -coverprofile=cov.out

.PHONY: all cover
