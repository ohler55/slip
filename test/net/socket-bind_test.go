// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"syscall"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSocketBindInet4(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("port", slip.Fixnum(availablePort()))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-socket :domain :inet :type :stream)))
                  (socket-bind sock #(127 0 0 1) port))`,
		Expect: "nil",
	}).Test(t)
}

func TestSocketBindOctets(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("port", slip.Fixnum(availablePort()))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-socket :domain :inet :type :stream)))
                  (socket-bind sock (coerce #(127 0 0 1) 'octets) port))`,
		Expect: "nil",
	}).Test(t)
}

func TestSocketBindInet6(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("port", slip.Fixnum(availablePort()))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-socket :domain :inet6 :type :stream)))
                  (socket-bind sock "::1" port))`,
		Expect: "nil",
	}).Test(t)
}

func TestSocketBindUnix(t *testing.T) {
	defer func() { _ = syscall.Unlink("quux") }()
	(&sliptest.Function{
		Source: `(let ((sock (make-socket :domain :local :type :stream)))
                  (send sock :bind "quux"))`,
		Expect: "nil",
	}).Test(t)
}

func TestSocketBindNotSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-bind t "quux")`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSocketBindNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((sock (make-socket :domain :local :type :stream)))
                  (socket-bind sock t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSocketBindBadAddress(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((sock (make-socket :domain :local :type :stream)))
                  (socket-bind sock))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((sock (make-socket :domain :inet :type :stream)))
                  (socket-bind sock "127.0.0.1" t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((sock (make-socket :domain :inet :type :stream)))
                  (socket-bind sock t 5))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((sock (make-socket :domain :inet :type :stream)))
                  (socket-bind sock #(127 0 0) 5))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSocketBindNoInit(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-bind (make-instance 'socket) "quux")`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSocketBindBindError(t *testing.T) {
	defer func() { _ = syscall.Unlink("quux") }()
	(&sliptest.Function{
		Source: `(let ((sock (make-socket :domain :inet :type :stream)))
                  (send sock :bind "quux"))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
