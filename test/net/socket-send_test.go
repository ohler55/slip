// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"fmt"
	"runtime"
	"syscall"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSocketSendOk(t *testing.T) {
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
                  (socket-send sock "hello" :timeout 1.0))`,
		Expect: "5",
	}).Test(t)
	buf := make([]byte, 10)
	cnt, err := syscall.Read(fds[1], buf)
	tt.Nil(t, err)
	tt.Equal(t, 5, cnt)
	tt.Equal(t, "hello", string(buf[:cnt]))
}

func TestUsocketSendSend(t *testing.T) {
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
                  (send sock :send (coerce "hello" 'octets) 3 :offset 1))`,
		Expect: "3",
	}).Test(t)
	buf := make([]byte, 10)
	cnt, err := syscall.Read(fds[1], buf)
	tt.Nil(t, err)
	tt.Equal(t, 3, cnt)
	tt.Equal(t, "ell", string(buf[:cnt]))
}

func TestSocketSendError(t *testing.T) {
	// Linux seems happy indicate a socket is ready for writing even when the
	// write buffer is full.
	// if runtime.GOOS != "darwin" {
	// 	return
	// }
	fmt.Printf("*** goos: %s\n", runtime.GOOS)
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
	}()
	err := syscall.SetsockoptInt(fds[0], syscall.SOL_SOCKET, syscall.SO_SNDBUF, 5)
	tt.Nil(t, err)
	foo, _ := syscall.GetsockoptInt(fds[0], syscall.SOL_SOCKET, syscall.SO_SNDBUF)
	fmt.Printf("*** size: %d\n", foo)
	_, err = syscall.Write(fds[0], []byte("hello"))
	tt.Nil(t, err)
	scope := slip.NewScope()
	scope.Let("ufd", slip.Fixnum(fds[0]))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'usocket :socket ufd)))
                  (socket-send sock "hello again" :timeout 0.001))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSocketSendBadLength(t *testing.T) {
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
                  (socket-send sock "hello" -1))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSocketSendBadOffset(t *testing.T) {
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
                  (socket-send sock "hello" :offset 6))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSocketSendNotOctets(t *testing.T) {
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
                  (socket-send sock t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSocketSendBrokenPipe(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
	}()
	scope := slip.NewScope()
	scope.Let("ufd", slip.Fixnum(fds[0]))
	_ = syscall.Close(fds[1])
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'usocket :socket ufd)))
                  (socket-send sock "hello"))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSocketSendTimeout(t *testing.T) {
	// Pick some random high port number which should error on select.
	(&sliptest.Function{
		Source: `(let ((sock (make-instance 'usocket :socket 777)))
                  (socket-send sock "hello" :timeout 0.01))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
