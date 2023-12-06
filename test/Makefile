
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
	make -C bag
	make -C clos
	make -C net
	make -C repl
	make -C csv
	make -C xml
	make -C test
	$Q grep github cl/cov.out >> cov.out
	$Q grep github flavors/cov.out >> cov.out
	$Q grep github gi/cov.out >> cov.out
	$Q grep github bag/cov.out >> cov.out
	$Q grep github clos/cov.out >> cov.out
	$Q grep github net/cov.out >> cov.out
	$Q grep github repl/cov.out >> cov.out
	$Q grep github csv/cov.out >> cov.out
	$Q grep github xml/cov.out >> cov.out
	$Q grep github test/cov.out >> cov.out
	$Q go tool cover -func=cov.out | grep "total:"

local:
	go test -coverpkg \
github.com/ohler55/slip,\
github.com/ohler55/slip/sliptest\
 -coverprofile=cov.out

.PHONY: all cover
