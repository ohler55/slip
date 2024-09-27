// Copyright (c) 2024, Peter Ohler, All rights reserved.

//go:build linux

package net_test

import (
	"syscall"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSocketSetOption(t *testing.T) {
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
                  (progn
                   (send sock :set-option :tcp-keepalive t)
                   ;; (send sock :set-option :tcp-nodelay t)
                   (send sock :set-option :broadcast t)
                   (send sock :set-option :reuse-address t)
                   (send sock :set-option :send-timeout 1.2)
                   (send sock :set-option :send-buffer 4095)
                   (send sock :set-option :receive-timeout 2)
                   (send sock :set-option :receive-buffer 4096))
                  (list
                   (socket-option sock :tcp-keepalive)
                   ;; (socket-option sock :tcp-nodelay)
                   (socket-option sock :broadcast)
                   (socket-option sock :reuse-address)
                   (socket-option sock :send-timeout)
                   (socket-option sock :send-buffer)
                   (socket-option sock :receive-timeout)
                   (socket-option sock :receive-buffer)))`,
		Expect: "(t t t 1 8190 2 8192)",
	}).Test(t)
}
