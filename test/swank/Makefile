
all: cover

# Run both the external integration tests (this directory) and the
# internal unit tests (pkg/swank/*_test.go at package swank), then
# merge the two coverage profiles so private helpers reached only
# from internal tests are counted.
cover:
	go test -covermode=atomic -coverpkg github.com/ohler55/slip/pkg/swank -coverprofile=cov-external.out
	go test -covermode=atomic -coverpkg github.com/ohler55/slip/pkg/swank -coverprofile=cov-internal.out ../../pkg/swank/
	echo "mode: atomic" > cov.out
	tail -n +2 cov-external.out >> cov.out
	tail -n +2 cov-internal.out >> cov.out
	go tool cover -func=cov.out | tail -1

.PHONY: all cover
