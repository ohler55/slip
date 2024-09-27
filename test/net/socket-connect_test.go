// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"syscall"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSocketConnectOkay(t *testing.T) {
	scope := slip.NewScope()
	port := availablePort()
	scope.Let("port", slip.Fixnum(port))
	ready := make(chan bool, 1)
	go func() {
		fd, err := syscall.Socket(syscall.AF_INET, syscall.SOCK_STREAM, 0)
		tt.Nil(t, err)
		err = syscall.Bind(fd, &syscall.SockaddrInet4{Port: port, Addr: [4]byte{127, 0, 0, 1}})
		tt.Nil(t, err)
		err = syscall.Listen(fd, 100)
		tt.Nil(t, err)
		ready <- true
		var (
			nfd int
			cnt int
		)
		nfd, _, err = syscall.Accept(fd)
		tt.Nil(t, err)
		buf := make([]byte, 20)
		cnt, err = syscall.Read(nfd, buf)
		tt.Nil(t, err)
		tt.Equal(t, "Hello", string(buf[:cnt]))
		_, err = syscall.Write(nfd, []byte("Goodbye"))
		tt.Nil(t, err)
	}()
	<-ready
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-socket :domain :inet :type :stream))
                       reply)
                  (socket-connect sock #(127 0 0 1) port)
                  (socket-send sock "Hello")
                  (setq reply (coerce (socket-receive sock nil 7) 'string))
                  (socket-close sock)
                  reply)`,
		Expect: `"Goodbye"`,
	}).Test(t)
}

func TestSocketSendConnect(t *testing.T) {
	scope := slip.NewScope()
	port := availablePort()
	scope.Let("port", slip.Fixnum(port))
	ready := make(chan bool, 1)
	go func() {
		fd, err := syscall.Socket(syscall.AF_INET, syscall.SOCK_STREAM, 0)
		tt.Nil(t, err)
		err = syscall.Bind(fd, &syscall.SockaddrInet4{Port: port, Addr: [4]byte{127, 0, 0, 1}})
		tt.Nil(t, err)
		err = syscall.Listen(fd, 100)
		tt.Nil(t, err)
		ready <- true
		var (
			nfd int
			cnt int
		)
		nfd, _, err = syscall.Accept(fd)
		tt.Nil(t, err)
		buf := make([]byte, 20)
		cnt, err = syscall.Read(nfd, buf)
		tt.Nil(t, err)
		tt.Equal(t, "Hello", string(buf[:cnt]))
		_, err = syscall.Write(nfd, []byte("Goodbye"))
		tt.Nil(t, err)
	}()
	<-ready
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-socket :domain :inet :type :stream))
                       reply)
                  (send sock :connect #(127 0 0 1) port)
                  (send sock :send "Hello")
                  (setq reply (coerce (send sock :receive nil 7) 'string))
                  (send sock :close)
                  reply)`,
		Expect: `"Goodbye"`,
	}).Test(t)
}

func TestSocketConnectNotSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-connect t #(127 0 0 1) 1111)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSocketConnectBadSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-connect (make-instance 'socket :domain :unix :type :stream) #(127 0 0 1) 1111)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
