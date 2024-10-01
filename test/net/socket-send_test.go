// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"bytes"
	"runtime"
	"syscall"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/gi"
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
		Source: `(let ((sock (make-instance 'socket :socket ufd)))
                  (socket-send sock "hello" :timeout 1.0))`,
		Expect: "5",
	}).Test(t)
	buf := make([]byte, 10)
	cnt, err := syscall.Read(fds[1], buf)
	tt.Nil(t, err)
	tt.Equal(t, 5, cnt)
	tt.Equal(t, "hello", string(buf[:cnt]))
}

func TestSocketSendSend(t *testing.T) {
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
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
	}()
	err := syscall.SetsockoptInt(fds[0], syscall.SOL_SOCKET, syscall.SO_SNDBUF, 5)
	tt.Nil(t, err)
	// Linux doesn't support setting the buffer size less than 4608 and it can
	// also double the size specified so to eliminate any oddness the size is
	// retrieved. It isn't correct for linux but rather double the actual so
	// use the size to pack the buffer.
	size, _ := syscall.GetsockoptInt(fds[0], syscall.SOL_SOCKET, syscall.SO_SNDBUF)
	_, err = syscall.Write(fds[0], bytes.Repeat([]byte{'x'}, size/2-1))
	tt.Nil(t, err)
	scope := slip.NewScope()
	scope.Let("ufd", slip.Fixnum(fds[0]))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'socket :socket ufd)))
                  (socket-send sock "hello" :timeout 0.001))`,
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
		Source: `(let ((sock (make-instance 'socket :socket ufd)))
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
		Source: `(let ((sock (make-instance 'socket :socket ufd)))
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
		Source: `(let ((sock (make-instance 'socket :socket ufd)))
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
		Source: `(let ((sock (make-instance 'socket :socket ufd)))
                  (socket-send sock "hello"))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSocketSendTimeout(t *testing.T) {
	// Pick some random high port number which should error on select.
	(&sliptest.Function{
		Source: `(let ((sock (make-instance 'socket :socket 777)))
                  (socket-send sock "hello" :timeout 0.01))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSocketSendBadFd(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((sock (make-instance 'socket :socket 777))
                       (buf (make-array 8 :element-type 'octet)))
                  (socket-send sock "hello"))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSocketSendInetDatagram(t *testing.T) {
	port := availablePort()
	sa := &syscall.SockaddrInet4{Port: port, Addr: [4]byte{127, 0, 0, 1}}
	ready := make(gi.Channel, 1)
	out := make(chan error, 1)
	go func() {
		fd, err := syscall.Socket(syscall.AF_INET, syscall.SOCK_DGRAM, 0)
		if err == nil {
			defer func() { _ = syscall.Close(fd) }()
			err = syscall.Bind(fd, sa)
			if err == nil {
				ready <- slip.True
				buf := make([]byte, 20)
				_, _, err = syscall.Recvfrom(fd, buf, 0)
			}
		}
		out <- err
	}()
	scope := slip.NewScope()
	scope.Let("port", slip.Fixnum(port))
	scope.Let("ready", ready)
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'socket :domain :inet :type :datagram))
                       result)
                  (channel-pop ready)
                  (setq result (socket-send sock "Hello" :timeout 1.0 :address (list "127.0.0.1" port)))
                  (socket-close sock)
                  result)`,
		Expect: "5",
	}).Test(t)
	err := <-out
	tt.Nil(t, err)
}

func TestSocketSendDatagramFlags(t *testing.T) {
	port := availablePort()
	sa := &syscall.SockaddrInet4{Port: port, Addr: [4]byte{127, 0, 0, 1}}
	ready := make(gi.Channel, 1)
	out := make(chan error, 1)
	go func() {
		fd, err := syscall.Socket(syscall.AF_INET, syscall.SOCK_DGRAM, 0)
		if err == nil {
			defer func() { _ = syscall.Close(fd) }()
			err = syscall.Bind(fd, sa)
			if err == nil {
				ready <- slip.True
				buf := make([]byte, 20)
				_, _, err = syscall.Recvfrom(fd, buf, 0)
			}
		}
		out <- err
	}()
	scope := slip.NewScope()
	scope.Let("port", slip.Fixnum(port))
	scope.Let("ready", ready)
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'socket :domain :inet :type :datagram))
                       result)
                  (channel-pop ready)
                  (setq result (socket-send sock "Hello"
                                           :timeout 1.0
                                           :offset 1
                                           :address (list "127.0.0.1" port)
                                           :eor t
                                           :dontwait t
                                           :dontroute t))
                  (socket-close sock)
                  result)`,
		Expect: "4",
	}).Test(t)
	err := <-out
	tt.Nil(t, err)
}

func TestSocketSendUnixDatagram(t *testing.T) {
	syscall.Unlink("uni")
	defer func() { _ = syscall.Unlink("uni") }()
	sa := &syscall.SockaddrUnix{Name: "uni"}
	ready := make(gi.Channel, 1)
	out := make(chan error, 1)
	go func() {
		fd, err := syscall.Socket(syscall.AF_UNIX, syscall.SOCK_DGRAM, 0)
		if err == nil {
			defer func() { _ = syscall.Close(fd) }()
			err = syscall.Bind(fd, sa)
			if err == nil {
				ready <- slip.True
				buf := make([]byte, 20)
				_, _, err = syscall.Recvfrom(fd, buf, 0)
			}
		}
		out <- err
	}()
	scope := slip.NewScope()
	scope.Let("ready", ready)
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'socket :domain :unix :type :datagram))
                       result)
                  (channel-pop ready)
                  (setq result (socket-send sock "Hello" :timeout 1.0 :address "uni"))
                  (socket-close sock)
                  result)`,
		Expect: "5",
	}).Test(t)
	err := <-out
	tt.Nil(t, err)
}

func TestSocketSendDatagramBadFlags(t *testing.T) {
	if runtime.GOOS == "darwin" {
		(&sliptest.Function{
			Source: `(let ((sock (make-instance 'socket :domain :inet :type :datagram)))
                  (socket-send sock "Hello" :address '("127.0.0.1" 7777) :oob t))`,
			PanicType: slip.ErrorSymbol,
		}).Test(t)
		(&sliptest.Function{
			Source: `(let ((sock (make-instance 'socket :domain :inet :type :datagram)))
                  (socket-send sock "Hello" :address '("127.0.0.1" 7777) :nosignal t))`,
			PanicType: slip.ErrorSymbol,
		}).Test(t)
		(&sliptest.Function{
			Source: `(let ((sock (make-instance 'socket :domain :inet :type :datagram)))
                  (socket-send sock "Hello" :address '("127.0.0.1" 7777) :confirm t))`,
			PanicType: slip.ErrorSymbol,
		}).Test(t)
	}
}

func TestSocketSendDatagramNoAddress(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((sock (make-instance 'socket :domain :inet :type :datagram)))
                  (socket-send sock "Hello"))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSocketSendDatagramBadAddress(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((sock (make-instance 'socket :domain :inet :type :datagram)))
                  (socket-send sock "Hello" :address t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
