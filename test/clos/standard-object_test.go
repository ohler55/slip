// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestStandardObjectBasic(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`
(defclass buux () ((z :initarg :z)))
(defclass quux (buux)
  (x (y :initarg :y :initform 1))
  (:default-initargs :y 3 :z 4))`, scope).Eval(scope, nil)
	scope.Let("qube", nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(setq qube (make-instance 'quux :y 5))`,
		Expect: `/#<quux [0-9a-f]+>/`,
	}).Test(t)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(describe qube)`,
		Expect: ``,
	}).Test(t)

	// TBD verify slots set
}
