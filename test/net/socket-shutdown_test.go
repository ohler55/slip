// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"runtime"
	"syscall"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSocketShutdownInput(t *testing.T) {
	if runtime.GOOS == "linux" {
		t.Skip()
	}
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
		Source: `(let ((sock (make-instance 'socket :socket ufd))
                       (buf (make-array 8 :element-type 'octet)))
                  (socket-shutdown sock :direction :input)
                  (socket-receive sock buf))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSocketSendShutdownOutput(t *testing.T) {
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
                  (send sock :shutdown :direction :output)
                  (socket-send sock "hello"))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSocketShutdownIO(t *testing.T) {
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
                  (socket-shutdown sock :direction :io)
                  (socket-send sock "hello"))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSocketShutdownBadDirection(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
	}()
	scope := slip.NewScope()
	scope.Let("ufd", slip.Fixnum(fds[0]))
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(socket-shutdown (make-instance 'socket :socket ufd) :direction :quux)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSocketShutdownNotSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-shutdown t :direction :quux)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSocketShutdownBadSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-shutdown (make-instance 'socket :socket 777) :direction :io)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
