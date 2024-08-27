// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"syscall"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSocketClose(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	defer func() {
		err := syscall.Close(fds[0])
		tt.NotNil(t, err) // should already be closed
		_ = syscall.Close(fds[1])
	}()
	scope := slip.NewScope()
	scope.Let("ufd", slip.Fixnum(fds[0]))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'usocket :socket ufd)))
                  (list (socket-close sock) (send sock :state)))`,
		Expect: "(nil nil)",
	}).Test(t)
}

func TestSocketSendClose(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	defer func() {
		err := syscall.Close(fds[0])
		tt.NotNil(t, err) // should already be closed
		_ = syscall.Close(fds[1])
	}()
	scope := slip.NewScope()
	scope.Let("ufd", slip.Fixnum(fds[0]))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'usocket :socket ufd)))
                  (list (send sock :close) (send sock :state)))`,
		Expect: "(nil nil)",
	}).Test(t)
}

func TestSocketCloseNotSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-close 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
