// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"syscall"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSockoptDebugGet(t *testing.T) {
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
                   (sockopt-debug sock))`,
		Expect: "nil",
	}).Test(t)
}

func TestSockoptDebugNotSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(sockopt-debug (make-instance 'vanilla-flavor))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSockoptDebugSetfNotSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(setf (sockopt-debug (make-instance 'vanilla-flavor)) t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSockoptDebugSetfBadSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(setf (sockopt-debug (make-instance 'socket :socket 7777)) t)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
