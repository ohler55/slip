// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"syscall"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSocketStream(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
	}()
	scope := slip.NewScope()
	scope.Let("ufd", slip.Fixnum(fds[0]))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'socket :socket ufd)))
                  (socket-stream sock))`,
		Expect: "#<IO-STREAM>",
	}).Test(t)
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'socket)))
                  (socket-stream sock))`,
		Expect: "nil",
	}).Test(t)
}

func TestSocketSendStream(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
	}()
	scope := slip.NewScope()
	scope.Let("ufd", slip.Fixnum(fds[0]))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'socket :socket ufd)))
                  (send sock :stream))`,
		Expect: "#<IO-STREAM>",
	}).Test(t)
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'socket)))
                  (send sock :stream))`,
		Expect: "nil",
	}).Test(t)
}

func TestSocketStreamRW(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
	}()
	scope := slip.NewScope()
	scope.Let("ufd0", slip.Fixnum(fds[0]))
	scope.Let("ufd1", slip.Fixnum(fds[1]))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock0 (make-instance 'socket :socket ufd0))
                       (sock1 (make-instance 'socket :socket ufd1)))
                  (format (socket-stream sock0) "hello~%")
                  (read (socket-stream sock1)))`,
		Expect: "hello",
	}).Test(t)
}

func TestSocketStreamNotSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-stream 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
