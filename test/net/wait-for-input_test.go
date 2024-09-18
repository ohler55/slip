// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"syscall"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestWaitForInputFound(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
	}()
	_, err := syscall.Write(fds[1], []byte("hello"))
	tt.Nil(t, err)
	scope := slip.NewScope()
	scope.Let("fd0", slip.Fixnum(fds[0]))
	scope.Let("fd1", slip.Fixnum(fds[1]))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock0 (make-instance 'socket :socket fd0))
                       (sock1 (make-instance 'socket :socket fd1)))
                  (wait-for-input (list sock0 sock1) :timeout 1))`,
		Validate: func(t *testing.T, v slip.Object) {
			values := v.(slip.Values)

			ready := values[0].(slip.List)
			tt.Equal(t, 1, len(ready))
			inst := ready[0].(*flavors.Instance)
			tt.Equal(t, fds[0], inst.Any)

			remain := values[1].(slip.DoubleFloat)
			tt.Equal(t, true, remain < 1.0)
		},
	}).Test(t)
}

func TestWaitForInputTimeout(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
	}()
	scope := slip.NewScope()
	scope.Let("fd0", slip.Fixnum(fds[0]))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock0 (make-instance 'socket :socket fd0)))
                  (wait-for-input sock0 :timeout 0.01))`,
		Expect: "nil, nil",
	}).Test(t)
}

func TestWaitForInputNilReadyOnly(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
	}()
	scope := slip.NewScope()
	scope.Let("fd0", slip.Fixnum(fds[0]))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock0 (make-instance 'socket :socket fd0)))
                  (wait-for-input sock0 :timeout 0.01 :ready-only nil))`,
		Validate: func(t *testing.T, v slip.Object) {
			values := v.(slip.Values)
			ready := values[0].(slip.List)
			tt.Equal(t, 1, len(ready))
			inst := ready[0].(*flavors.Instance)
			tt.Equal(t, fds[0], inst.Any)
			tt.Nil(t, values[1])
		},
	}).Test(t)
}

func TestWaitForInputNotSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(wait-for-input t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(wait-for-input '(t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWaitForInputBadTimeout(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
	}()
	scope := slip.NewScope()
	scope.Let("fd0", slip.Fixnum(fds[0]))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock0 (make-instance 'socket :socket fd0)))
                  (wait-for-input sock0 :timeout -0.01))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
