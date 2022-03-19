
all: cover

cover:
	go test -coverpkg \
github.com/ohler55/slip,\
github.com/ohler55/slip/pkg/basic,\
github.com/ohler55/slip/pkg/hash,\
github.com/ohler55/slip/pkg/list,\
github.com/ohler55/slip/sliptest\
 -coverprofile=cov.out

.PHONY: all cover
