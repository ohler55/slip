// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"syscall"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSocketOpenp(t *testing.T) {
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
		Source: `(let* ((sock (make-instance 'socket :socket ufd))
                        (openp (socket-open-p sock)))
                  (socket-close sock)
                  (list openp (socket-open-p sock)))`,
		Expect: "(t nil)",
	}).Test(t)
}

func TestSocketSendOpenp(t *testing.T) {
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
		Source: `(let* ((sock (make-instance 'socket :socket ufd))
                        (openp (send sock :open-p)))
                  (send sock :close)
                  (list openp (send sock :open-p)))`,
		Expect: "(t nil)",
	}).Test(t)
}

func TestSocketOpenpNotSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-open-p 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
