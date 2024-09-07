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
		Source: `(let ((sock (make-instance 'usocket :socket ufd)))
                  (socket-stream sock))`,
		Expect: "#<IO-STREAM>",
	}).Test(t)
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'usocket)))
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
		Source: `(let ((sock (make-instance 'usocket :socket ufd)))
                  (send sock :stream))`,
		Expect: "#<IO-STREAM>",
	}).Test(t)
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'usocket)))
                  (send sock :stream))`,
		Expect: "nil",
	}).Test(t)
}

func TestSocketStreamNotSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-stream 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
