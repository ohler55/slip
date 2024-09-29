// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
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

// func TestSocketReceiveTimeout(t *testing.T) {
// 	// Pick some random high port number which should error on select.
// 	(&sliptest.Function{
// 		Source: `(let ((sock (make-instance 'socket :socket 777)))
//                   (socket-receive sock :timeout 0.001))`,
// 		PanicType: slip.ErrorSymbol,
// 	}).Test(t)
// }

// func TestSocketReceiveDatagram(t *testing.T) {
// 	scope := slip.NewScope()
// 	port := availablePort()
// 	scope.Let("port", slip.Fixnum(port))
// 	go func() {
// 		fd, err := syscall.Socket(syscall.AF_INET, syscall.SOCK_DGRAM, 0)
// 		tt.Nil(t, err)
// 		fmt.Printf("*** created\n")

// 		sa := &syscall.SockaddrInet4{Port: port, Addr: [4]byte{127, 0, 0, 1}}
// 		err = syscall.Bind(fd, sa)
// 		tt.Nil(t, err)
// 		fmt.Printf("*** bind: %s\n", err)

// 		time.Sleep(time.Second)

// 		err = syscall.Sendmsg(fd, []byte("hello"), nil, sa, 0)
// 		tt.Nil(t, err)
// 	}()
// 	fmt.Printf("*** ready to test\n")
// 	(&sliptest.Function{
// 		Scope: scope,
// 		Source: `(let ((sock (make-socket :domain :inet :type :datagram))
//                        reply)
//                   (socket-connect sock #(127 0 0 1) port)
//                   (setq reply (coerce (socket-receive sock nil 7) 'string))
//                   (socket-close sock)
//                   reply)`,
// 		Expect: `"Goodbye"`,
// 	}).Test(t)
// }

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
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'socket :domain :inet :type :datagram))
                       (buf (make-array 8 :element-type 'octet)))
                  (socket-bind sock #(127 0 0 1) port)
                  (channel-push ready t)
                  (socket-receive sock buf 8 :timeout 1.0 :oob t))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
