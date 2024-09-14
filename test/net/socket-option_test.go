// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"syscall"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSocketOptionDefaults(t *testing.T) {
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
                  (list
                   (socket-option sock :tcp-keepalive)
                   ;; (socket-option sock :tcp-nodelay)
                   (socket-option sock :broadcast)
                   (socket-option sock :reuse-address)
                   (socket-option sock :send-timeout)
                   (socket-option sock :send-buffer)
                   (socket-option sock :receive-timeout)
                   (socket-option sock :receive-buffer)))`,
		Expect: `/^\(nil nil nil 0 [0-9]+ 0 [0-9]+\)$/`,
	}).Test(t)
}

func TestUsocketOptionSend(t *testing.T) {
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
                  (send sock :option :tcp-keepalive))`,
		Expect: "nil",
	}).Test(t)
}

func TestUsocketSetfOption(t *testing.T) {
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
                  (setf (send sock :option :tcp-keepalive) t)
                  (send sock :option :tcp-keepalive))`,
		Expect: "t",
	}).Test(t)
}

func TestSocketOptionSetf(t *testing.T) {
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
                  (setf (socket-option sock :tcp-keepalive) t)
                  (socket-option sock :tcp-keepalive))`,
		Expect: "t",
	}).Test(t)
}

func TestSocketOptionBadOption(t *testing.T) {
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
                  (socket-option sock :bad))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSocketSetOptionBadOption(t *testing.T) {
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
                  (send sock :set-option :bad t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestUsocketSetOptionNotFixnum(t *testing.T) {
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
                  (send sock :set-option :send-buffer t))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestUsocketSetOptionNotTime(t *testing.T) {
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
                  (send sock :set-option :send-timeout t))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSocketOptionNotSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-option (make-instance 'vanilla-flavor) :tcp-keepalive)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSocketOptionSetfNotSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(setf (socket-option (make-instance 'vanilla-flavor) :tcp-keepalive) t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestUsocketOptionSendBadSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-option (make-instance 'usocket :socket 777) :tcp-keepalive)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(send (make-instance 'usocket :socket 777) :option :tcp-keepalive t)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
