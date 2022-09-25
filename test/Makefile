
Q = $(if $(filter 1,$V),,@)

all: cover

cover:
	go test -coverpkg \
github.com/ohler55/slip,\
github.com/ohler55/slip/sliptest\
 -coverprofile=cov.out
	make -C cl
	make -C flavors
	make -C gi
	$Q grep github cl/cov.out >> cov.out
	$Q grep github flavors/cov.out >> cov.out
	$Q grep github gi/cov.out >> cov.out
	$Q grep github bag/cov.out >> cov.out
	$Q go tool cover -func=cov.out | grep "total:"

local:
	go test -coverpkg \
github.com/ohler55/slip,\
github.com/ohler55/slip/sliptest\
 -coverprofile=cov.out

.PHONY: all cover
