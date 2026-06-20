Q = $(if $(filter 1,$V),,@)

all: cover

cover:
	go run ../cmd/slipr/main.go cover-test.lisp -v

.PHONY: all cover
