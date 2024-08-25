// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"syscall"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSocketState(t *testing.T) {
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
                  (list (send sock :state) (socket-state sock)))`,
		Expect: "(:read-write :read-write)",
	}).Test(t)
}
