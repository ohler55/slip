
all: cover

cover:
	go test -coverpkg \
github.com/ohler55/slip,\
github.com/ohler55/slip/sliptest\
 -coverprofile=cov.out
	make -C cl
	make -C flavors

.PHONY: all cover
