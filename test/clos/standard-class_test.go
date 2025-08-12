// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos_test

import (
	"bytes"
	"sort"
	"strings"
	"testing"

	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/clos"
	"github.com/ohler55/slip/pp"
)

func TestStandardClassBasic(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(progn (makunbound 'quux) (makunbound 'buux))`, scope).Eval(scope, nil)
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
`, string(pp.Append(nil, scope, quux.LoadForm())))

	tt.Equal(t, false, quux.Ready())
	tt.Panic(t, func() { _ = quux.MakeInstance() })

	buux := slip.ReadString(`
(defclass buux ()
  ((z
    :initarg :z
    :initarg :zz
    :initform 3
    :reader get-z
    :writer set-z
    :accessor z-access
    :type fixnum)))`, scope).Eval(scope, nil).(*clos.StandardClass)
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

	tt.Equal(t, true, quux.Inherits(buux))
	tt.Equal(t, false, quux.Inherits(slip.FindClass("vanilla-flavor")))

	desc := quux.Describe(nil, 0, 80, false)
	tt.Equal(t, `quux is a class:
  Documentation:
    quack quack
  Direct superclasses: buux
  Class precedence list: quux buux standard-object t
  Slots:
    x
    y
      initargs: :y
    z (buux)
      initargs: :z :zz
      initform: 3
      readers: get-z
      writers: set-z
      accessors: z-access
      type: fixnum
  Default-initargs:
    :y: 3
  Direct Methods:
    :class
    :describe
    :equal
    :eval-inside-yourself
    :id
    :init
    :inspect
    :operation-handled-p
    :print-self
    :send-if-handles
    :shared-initialize
    :which-operations
`, string(desc))

	desc = quux.Describe(nil, 0, 80, true)
	tt.Equal(t, true, bytes.Contains(desc, []byte{0x1b, '[', '1', 'm'}))

	tt.Equal(t, []slip.Class{slip.FindClass("buux")}, quux.InheritsList())
	tt.Equal(t, slip.Symbol("standard-class"), quux.Metaclass())
	names := quux.MethodNames().String()
	tt.Equal(t, true, strings.Contains(names, ":class"))
	tt.Equal(t, true, strings.Contains(names, ":describe"))
	tt.Equal(t, true, strings.Contains(names, ":print-self"))
	vars := quux.VarNames()
	sort.Strings(vars)
	tt.Equal(t, []string{"x", "y", "z"}, vars)
}

func TestStandardClassAllocClass(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(makunbound 'quux)`, scope).Eval(scope, nil)
	quux := slip.ReadString(`
(defclass quux ()
  ((x :initarg :x :initform 3 :allocation :class)))`, scope).Eval(scope, nil).(*clos.StandardClass)

	desc := quux.Describe(nil, 0, 80, false)
	tt.Equal(t, `quux is a class:
  Direct superclasses:
  Class precedence list: quux standard-object t
  Slots:
    x
      initargs: :x
      initform: 3
      allocation: class
  Class Slots:
    x = 3
  Direct Methods:
    :class
    :describe
    :equal
    :eval-inside-yourself
    :id
    :init
    :inspect
    :operation-handled-p
    :print-self
    :send-if-handles
    :shared-initialize
    :which-operations
`, string(desc))
}

func TestStandardClassMinimal(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(makunbound 'quux)`, scope).Eval(scope, nil)
	quux := slip.ReadString(`(defclass quux () ())`, scope).Eval(scope, nil).(*clos.StandardClass)
	desc := quux.Describe(nil, 0, 80, false)

	tt.Equal(t, `quux is a class:
  Direct superclasses:
  Class precedence list: quux standard-object t
  Slots: None
  Direct Methods:
    :class
    :describe
    :equal
    :eval-inside-yourself
    :id
    :init
    :inspect
    :operation-handled-p
    :print-self
    :send-if-handles
    :shared-initialize
    :which-operations
`, string(desc))
}

func TestStandardClassMergeSupers(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(progn (makunbound 'quux) (makunbound 'buux) (makunbound 'duux))`, scope).Eval(scope, nil)
	quux := slip.ReadString(`(defclass quux (buux duux) ())`, scope).Eval(scope, nil).(*clos.StandardClass)
	_ = slip.ReadString(`(defclass buux (duux) ())`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(defclass duux () ())`, scope).Eval(scope, nil)

	tt.Equal(t, `(defclass quux (buux duux)
  ())
`, string(pp.Append(nil, scope, quux.LoadForm())))

	tt.Equal(t, true, quux.Ready())
}
