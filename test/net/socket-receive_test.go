// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"syscall"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSocketReceiveBasic(t *testing.T) {
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
		Source: `(let ((sock (make-instance 'usocket :socket ufd))
                       (buf (make-array 8 :element-type 'octet)))
                  (socket-receive sock buf 8 :timeout 1.0))`,
		Expect: `#(104 101 108 108 111 0 0 0), 5, "@", 0`,
	}).Test(t)
}

func TestSocketReceiveNoBuffer(t *testing.T) {
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
		Source: `(let ((sock (make-instance 'usocket :socket ufd)))
                  (socket-receive sock nil 6))`,
		Expect: `#(104 101 108 108 111 0), 5, "@", 0`,
	}).Test(t)
}

func TestSocketReceiveNoLength(t *testing.T) {
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
		Source: `(let ((sock (make-instance 'usocket :socket ufd))
                       (buf (make-array 8 :element-type 'octet)))
                  (send sock :receive buf))`,
		Expect: `#(104 101 108 108 111 0 0 0), 5, "@", 0`,
	}).Test(t)
}

func TestSocketReceiveJustSocket(t *testing.T) {
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
		Source: `(let ((sock (make-instance 'usocket :socket ufd)))
                  (socket-receive sock))`,
		Validate: func(t *testing.T, v slip.Object) {
			tt.SameType(t, slip.Values{}, v)
			sv := v.(slip.Values)
			tt.SameType(t, slip.Octets{}, sv[0])
			tt.Equal(t, 65507, len(sv[0].(slip.Octets)))
		},
	}).Test(t)
}

func TestSocketReceiveEOF(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
	}()
	_ = syscall.Close(fds[1])
	scope := slip.NewScope()
	scope.Let("ufd", slip.Fixnum(fds[0]))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'usocket :socket ufd)))
                  (socket-receive sock))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSocketReceiveNotOctets(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
	}()
	_ = syscall.Close(fds[1])
	scope := slip.NewScope()
	scope.Let("ufd", slip.Fixnum(fds[0]))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'usocket :socket ufd)))
                  (socket-receive sock t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSocketReceiveBadLength(t *testing.T) {
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
		Source: `(let ((sock (make-instance 'usocket :socket ufd))
                       (buf (make-array 8 :element-type 'octet)))
                  (send sock :receive buf -1))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
