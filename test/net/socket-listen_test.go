// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSocketListenOkay(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("port", slip.Fixnum(availablePort()))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-socket :domain :inet :type :stream)))
                  (socket-bind sock #(127 0 0 1) port)
                  (socket-listen sock 100)
                  (socket-close sock))`,
		Expect: "nil",
	}).Test(t)
}

func TestSocketSendListen(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("port", slip.Fixnum(availablePort()))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-socket :domain :inet :type :stream)))
                  (send sock :bind #(127 0 0 1) port)
                  (send sock :listen 100)
                  (send sock :close))`,
		Expect: "nil",
	}).Test(t)
}

func TestSocketListenNotSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-listen t 100)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSocketListenBadBacklog(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("port", slip.Fixnum(availablePort()))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-socket :domain :inet :type :stream)))
                  (socket-bind sock #(127 0 0 1) port)
                  (socket-listen sock t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSocketListenNoInit(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-listen (make-instance 'socket) 100)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSocketListenBadSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-listen (make-instance 'socket :socket 777) 100)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
