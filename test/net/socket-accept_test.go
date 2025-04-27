// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"bufio"
	"fmt"
	"net"
	"testing"
	"time"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSocketAcceptOkay(t *testing.T) {
	scope := slip.NewScope()
	port := availablePort()
	scope.Let("port", slip.Fixnum(port))
	done := make(chan bool, 1)
	go func() {
		start := time.Now()
		for time.Since(start) < time.Second {
			c, err := net.Dial("tcp", fmt.Sprintf("127.0.0.1:%d", port))
			if err != nil {
				time.Sleep(time.Millisecond * 50)
				continue
			}
			_, _ = fmt.Fprintf(c, "Hello")
			var resp string
			resp, err = bufio.NewReader(c).ReadString('e')
			tt.Nil(t, err)
			tt.Equal(t, "Goodbye", resp)
			done <- true
			return
		}
		done <- false
	}()
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-socket :domain :inet :type :stream))
                       s2)
                  (socket-bind sock #(127 0 0 1) port)
                  (socket-listen sock 100)
                  (setq s2 (socket-accept sock))
                  (list
                   (coerce (socket-receive s2 nil 5) 'string)
                   (socket-send s2 "Goodbye")))`,
		Expect: `("Hello" 7)`,
	}).Test(t)
	if !<-done {
		t.FailNow()
	}
}

func TestSocketSendAccept(t *testing.T) {
	scope := slip.NewScope()
	port := availablePort()
	scope.Let("port", slip.Fixnum(port))
	done := make(chan bool, 1)
	go func() {
		start := time.Now()
		for time.Since(start) < time.Second {
			c, err := net.Dial("tcp", fmt.Sprintf("127.0.0.1:%d", port))
			if err != nil {
				time.Sleep(time.Millisecond * 50)
				continue
			}
			_, _ = fmt.Fprintf(c, "Hello")
			var resp string
			resp, err = bufio.NewReader(c).ReadString('e')
			tt.Nil(t, err)
			tt.Equal(t, "Goodbye", resp)
			done <- true
			return
		}
		done <- false
	}()
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-socket :domain :inet :type :stream))
                       s2)
                  (send sock :bind #(127 0 0 1) port)
                  (send sock :listen 100)
                  (setq s2 (send sock :accept))
                  (list
                   (coerce (send s2 :receive nil 5) 'string)
                   (send s2 :send "Goodbye")))`,
		Expect: `("Hello" 7)`,
	}).Test(t)
	if !<-done {
		t.FailNow()
	}
}

func TestSocketAcceptNotSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-accept t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSocketAcceptNotConnected(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-accept (make-instance 'socket))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSocketAcceptBadSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-accept (make-instance 'socket :socket 777))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
