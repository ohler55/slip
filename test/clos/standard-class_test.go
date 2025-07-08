// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/clos"
	"github.com/ohler55/slip/pp"
)

func TestStandardClass(t *testing.T) {
	scope := slip.NewScope()
	quux := slip.ReadString(`
(defclass quux (buux)
  (x
   (y :initarg :y))
  (:documentation "quack quack")
  (:default-initargs :y 3))`, scope).Eval(scope, nil).(*clos.StandardClass)

	tt.Equal(t, `(defclass quux (buux)
  (x (y :initarg :y))
  (:documentation "quack quack")
  (:default-initargs :y 3))
`, string(pp.Append(nil, scope, quux.DefList())))

	tt.Equal(t, false, quux.Ready())

	buux := slip.ReadString(`
(defclass buux ()
  (z :initarg :z))`, scope).Eval(scope, nil).(*clos.StandardClass)
	tt.Equal(t, true, buux.Ready())
	tt.Equal(t, true, quux.Ready())

	tt.Equal(t, "quack quack", quux.Documentation())
	tt.Equal(t, "", buux.Documentation())
	buux.SetDocumentation("buck buck")
	tt.Equal(t, "buck buck", buux.Documentation())

	tt.Equal(t, "#<standard-class quux>", quux.String())
	tt.Equal(t, false, quux.Equal(buux))

	tt.Equal(t, "[standard-class class standard-object t]", pretty.SEN(quux.Hierarchy()))
	tt.Equal(t, "quux", quux.Name())
	tt.Equal(t, true, quux == quux.Eval(nil, 0))
}
