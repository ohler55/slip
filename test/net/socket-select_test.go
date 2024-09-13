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

func TestSocketSelectBasic(t *testing.T) {
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
		Source: `(let ((sock0 (make-instance 'usocket :socket fd0))
                       (sock1 (make-instance 'usocket :socket fd1)))
                  (socket-select (list sock0 sock1) (list sock0 sock1) (list sock0 sock1) :timeout 1))`,
		Validate: func(t *testing.T, v slip.Object) {
			values := v.(slip.Values)
			tt.Equal(t, 3, len(values))

			read := values[0].(slip.List)
			tt.Equal(t, 1, len(read))
			inst := read[0].(*flavors.Instance)
			tt.Equal(t, fds[0], inst.Any)

			write := values[1].(slip.List)
			tt.Equal(t, 2, len(write))

			errs := values[2].(slip.List)
			tt.Equal(t, 0, len(errs))
		},
	}).Test(t)
}

func TestSocketSelectOne(t *testing.T) {
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
		Source: `(let ((sock0 (make-instance 'usocket :socket fd0))
                       (sock1 (make-instance 'usocket :socket fd1)))
                  (socket-select sock0 sock0 sock0 :timeout 1))`,
		Validate: func(t *testing.T, v slip.Object) {
			values := v.(slip.Values)
			tt.Equal(t, 3, len(values))

			read := values[0].(slip.List)
			tt.Equal(t, 1, len(read))
			inst := read[0].(*flavors.Instance)
			tt.Equal(t, fds[0], inst.Any)

			write := values[1].(slip.List)
			tt.Equal(t, 1, len(write))

			errs := values[2].(slip.List)
			tt.Equal(t, 0, len(errs))
		},
	}).Test(t)
}

func TestSocketSelectBadTimeout(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
	}()
	_, err := syscall.Write(fds[1], []byte("hello"))
	tt.Nil(t, err)
	scope := slip.NewScope()
	scope.Let("fd0", slip.Fixnum(fds[0]))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock0 (make-instance 'usocket :socket fd0)))
                  (socket-select sock0 sock0 sock0 :timeout t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSocketSelectNotSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-select t t t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((inst (make-instance 'vanilla-flavor)))
                  (socket-select inst inst inst))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
