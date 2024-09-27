// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"syscall"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSockoptKeepAliveGet(t *testing.T) {
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
                   (sockopt-keep-alive sock))`,
		Expect: "nil",
	}).Test(t)
}

func TestSockoptKeepAliveSetf(t *testing.T) {
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
                   (setf (sockopt-keep-alive sock) t)
                   (sockopt-keep-alive sock))`,
		Expect: "t",
	}).Test(t)
}

func TestSockoptKeepAliveNotSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(sockopt-keep-alive (make-instance 'vanilla-flavor))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSockoptKeepAliveSetfNotSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(setf (sockopt-keep-alive (make-instance 'vanilla-flavor)) t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSockoptKeepAliveSetfBadSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(setf (sockopt-keep-alive (make-instance 'socket :socket 7777)) t)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
