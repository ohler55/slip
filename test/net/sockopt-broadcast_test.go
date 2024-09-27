// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"syscall"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSockoptBroadcastGet(t *testing.T) {
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
                   (sockopt-broadcast sock))`,
		Expect: "nil",
	}).Test(t)
}

func TestSockoptBroadcastSetf(t *testing.T) {
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
                   (setf (sockopt-broadcast sock) t)
                   (sockopt-broadcast sock))`,
		Expect: "t",
	}).Test(t)
}

func TestSockoptBroadcastNotSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(sockopt-broadcast (make-instance 'vanilla-flavor))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSockoptBroadcastSetfNotSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(setf (sockopt-broadcast (make-instance 'vanilla-flavor)) t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSockoptBroadcastSetfBadSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(setf (sockopt-broadcast (make-instance 'socket :socket 7777)) t)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
