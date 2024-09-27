// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"syscall"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSocketMakeStream(t *testing.T) {
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
                  (socket-make-stream sock :input t :output t :timeout 1.2))`,
		Expect: "#<IO-STREAM>",
	}).Test(t)
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'socket)))
                  (socket-make-stream sock))`,
		Expect: "nil",
	}).Test(t)
}

func TestSocketSendMakeStreamInput(t *testing.T) {
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
                  (send sock :make-stream :input t))`,
		Expect: "#<INPUT-STREAM>",
	}).Test(t)
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'socket)))
                  (send sock :make-stream :input t))`,
		Expect: "nil",
	}).Test(t)
}

func TestSocketSendMakeStreamOutput(t *testing.T) {
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
                  (send sock :make-stream :output t))`,
		Expect: "#<OUTPUT-STREAM>",
	}).Test(t)
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'socket)))
                  (send sock :make-stream :output t))`,
		Expect: "nil",
	}).Test(t)
}

func TestSocketMakeStreamRW(t *testing.T) {
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
		Source: `(let* ((out (socket-make-stream (make-instance 'socket :socket ufd0) :output t))
                        (in (socket-make-stream (make-instance 'socket :socket ufd1) :input t)))
                  (format out "hello~%")
                  (read in))`,
		Expect: "hello",
	}).Test(t)
}

func TestSocketMakeStreamNotSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-make-stream 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSocketMakeStreamBadSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-make-stream (make-instance 'socket :socket 777) :timeout 1 :input t)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSocketMakeStreamNotIO(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-make-stream (make-instance 'socket :socket 777))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
