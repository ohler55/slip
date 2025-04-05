// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"runtime"
	"syscall"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/gi"
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
		Source: `(let ((sock (make-instance 'socket :socket ufd))
                       (buf (make-array 8 :element-type 'octet)))
                  (socket-receive sock buf 8 :timeout 1.0))`,
		Expect: `#(104 101 108 108 111 0 0 0), 5, ("@" 0)`,
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
		Source: `(let ((sock (make-instance 'socket :socket ufd)))
                  (socket-receive sock nil 6))`,
		Expect: `#(104 101 108 108 111 0), 5, ("@" 0)`,
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
		Source: `(let ((sock (make-instance 'socket :socket ufd))
                       (buf (make-array 8 :element-type 'octet)))
                  (send sock :receive buf))`,
		Expect: `#(104 101 108 108 111 0 0 0), 5, ("@" 0)`,
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
		Source: `(let ((sock (make-instance 'socket :socket ufd)))
                  (socket-receive sock))`,
		Validate: func(t *testing.T, v slip.Object) {
			tt.SameType(t, slip.Values{}, v)
			sv := v.(slip.Values)
			tt.SameType(t, slip.Octets{}, sv[0])
			tt.Equal(t, 65507, len(sv[0].(slip.Octets)))
		},
	}).Test(t)
}

func TestSocketReceiveTimeout(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
	}()
	scope := slip.NewScope()
	scope.Let("ufd", slip.Fixnum(fds[0]))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'socket :socket ufd))
                       (buf (make-array 8 :element-type 'octet)))
                  (socket-receive sock buf 8 :timeout 0.001))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSocketReceiveError(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((sock (make-instance 'socket :socket 777))
                       (buf (make-array 8 :element-type 'octet)))
                  (socket-receive sock buf 8 :timeout 0.001))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSocketReceiveBadFd(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((sock (make-instance 'socket :socket 777))
                       (buf (make-array 8 :element-type 'octet)))
                  (socket-receive sock buf 8))`,
		PanicType: slip.ErrorSymbol,
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
		Source: `(let ((sock (make-instance 'socket :socket ufd)))
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
		Source: `(let ((sock (make-instance 'socket :socket ufd)))
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
		Source: `(let ((sock (make-instance 'socket :socket ufd))
                       (buf (make-array 8 :element-type 'octet)))
                  (send sock :receive buf -1))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSocketReceiveUnixDatagram(t *testing.T) {
	fds, err0 := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_DGRAM, 0)
	tt.Nil(t, err0)
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
                  (socket-receive sock buf 8 :timeout 1.0))`,
		Expect: `#(104 101 108 108 111 0 0 0), 5, nil`,
	}).Test(t)
}

func TestSocketReceiveUnix2Datagram(t *testing.T) {
	_ = syscall.Unlink("unix2")
	defer func() { _ = syscall.Unlink("unix2") }()
	sa := &syscall.SockaddrUnix{Name: "unix2"}
	started := make(gi.Channel, 1)
	ready := make(gi.Channel, 1)
	out := make(chan error, 1)
	go func() {
		fd, err := syscall.Socket(syscall.AF_UNIX, syscall.SOCK_DGRAM, 0)
		if err == nil {
			defer func() { _ = syscall.Close(fd) }()
			started <- slip.True
			<-ready
			err = syscall.Sendmsg(fd, []byte("hello"), nil, sa, 0)
		}
		out <- err
	}()

	scope := slip.NewScope()
	scope.Let("ready", ready)
	scope.Let("started", started)
	(&sliptest.Function{
		Scope: scope,
		Array: true,
		Source: `(let ((sock (make-instance 'socket :domain :unix :type :datagram))
                       (buf (make-array 8 :element-type 'octet))
                       result)
                  (channel-pop started)
                  (socket-bind sock "unix2")
                  (channel-push ready t)
                  (setq result (socket-receive sock buf 8 :timeout 1.0))
                  (socket-close sock)
                  result)`,
		Expect: `#(104 101 108 108 111 0 0 0)`,
	}).Test(t)
	err := <-out
	tt.Nil(t, err)
}

func TestSocketReceiveInetDatagram(t *testing.T) {
	port := availablePort()
	sa := &syscall.SockaddrInet4{Port: port, Addr: [4]byte{127, 0, 0, 1}}
	ready := make(gi.Channel, 1)
	go func() {
		fd, err := syscall.Socket(syscall.AF_INET, syscall.SOCK_DGRAM, 0)
		tt.Nil(t, err)
		<-ready
		err = syscall.Sendmsg(fd, []byte("hello"), []byte{}, sa, 0)
		tt.Nil(t, err)
	}()
	scope := slip.NewScope()
	scope.Let("port", slip.Fixnum(port))
	scope.Let("ready", ready)
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'socket :domain :inet :type :datagram))
                       (buf (make-array 8 :element-type 'octet)))
                  (socket-bind sock #(127 0 0 1) port)
                  (channel-push ready t)
                  (socket-receive sock buf 8 :timeout 1.0))`,
		Expect: `/#\(104 101 108 108 111 0 0 0\), 5, \(#<\(VECTOR 4\)> [0-9]+\)/`,
	}).Test(t)
}

func TestSocketReceiveInet6Datagram(t *testing.T) {
	port := availablePort()
	sa := &syscall.SockaddrInet6{Port: port, Addr: [16]byte{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1}}
	ready := make(gi.Channel, 1)
	go func() {
		fd, err := syscall.Socket(syscall.AF_INET6, syscall.SOCK_DGRAM, 0)
		tt.Nil(t, err)
		<-ready
		err = syscall.Sendmsg(fd, []byte("hello"), []byte{}, sa, 0)
		tt.Nil(t, err)
	}()
	scope := slip.NewScope()
	scope.Let("port", slip.Fixnum(port))
	scope.Let("ready", ready)
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'socket :domain :inet6 :type :datagram))
                       (buf (make-array 8 :element-type 'octet)))
                  (socket-bind sock #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) port)
                  (channel-push ready t)
                  (socket-receive sock buf 8 :timeout 1.0 :waitall t :dontwait t :peek t))`,
		Expect: `/#\(104 101 108 108 111 0 0 0\), 5, \(#<\(VECTOR 16\)> [0-9]+\)/`,
	}).Test(t)
}

func TestSocketReceiveInetDatagramOob(t *testing.T) {
	port := availablePort()
	sa := &syscall.SockaddrInet4{Port: port, Addr: [4]byte{127, 0, 0, 1}}
	ready := make(gi.Channel, 1)
	go func() {
		fd, err := syscall.Socket(syscall.AF_INET, syscall.SOCK_DGRAM, 0)
		tt.Nil(t, err)
		<-ready
		err = syscall.Sendmsg(fd, []byte("hello"), []byte{}, sa, 0)
		tt.Nil(t, err)
	}()
	scope := slip.NewScope()
	scope.Let("port", slip.Fixnum(port))
	scope.Let("ready", ready)
	var (
		x  string
		pt slip.Object
	)
	switch runtime.GOOS {
	case "linux":
		x = `/#\(104 101 108 108 111 0 0 0\), 5, \(#<\(VECTOR 4\)> [0-9]+\)/`
	case "darwin":
		pt = slip.ErrorSymbol
	}
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'socket :domain :inet :type :datagram))
                       (buf (make-array 8 :element-type 'octet)))
                  (socket-bind sock #(127 0 0 1) port)
                  (channel-push ready t)
                  (socket-receive sock buf 8 :timeout 1.0 :oob t))`,
		Expect:    x,
		PanicType: pt,
	}).Test(t)
}
