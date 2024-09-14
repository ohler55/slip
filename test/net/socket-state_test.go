// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"bytes"
	"syscall"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSocketStateWrite(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
	}()
	scope := slip.NewScope()
	scope.Let("ufd", slip.Fixnum(fds[0]))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'usocket :socket ufd)))
                  (list (send sock :state) (socket-state sock)))`,
		Expect: "(:write :write)",
	}).Test(t)
}

func TestSocketStateReadWrite(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
	}()
	_, err := syscall.Write(fds[1], []byte("hello"))
	tt.Nil(t, err)
	scope := slip.NewScope()
	scope.Let("ufd", slip.Fixnum(fds[0]))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'usocket :socket ufd)))
                  (list (send sock :state) (socket-state sock)))`,
		Expect: "(:read-write :read-write)",
	}).Test(t)
}

func TestSocketStateRead(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
	}()
	err := syscall.SetsockoptInt(fds[0], syscall.SOL_SOCKET, syscall.SO_SNDBUF, 5)
	tt.Nil(t, err)
	// Linux doesn't support setting the buffer size less than 4608 and it can
	// also double the size specified so to eliminate any oddness the size is
	// retrieved. It isn't correct for linux but rather double the actual so
	// use the size to pack the buffer.
	size, _ := syscall.GetsockoptInt(fds[0], syscall.SOL_SOCKET, syscall.SO_SNDBUF)
	_, err = syscall.Write(fds[0], bytes.Repeat([]byte{'x'}, size/2-1))
	tt.Nil(t, err)

	_, err = syscall.Write(fds[1], []byte("hello"))
	tt.Nil(t, err)
	scope := slip.NewScope()
	scope.Let("ufd", slip.Fixnum(fds[0]))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'usocket :socket ufd)))
                  (list (send sock :state) (socket-state sock)))`,
		Expect: "(:read :read)",
	}).Test(t)
}
