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

func TestConditionClassBasic(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(progn (makunbound 'quux) (makunbound 'buux))`, scope).Eval(scope, nil)

	quux := slip.ReadString(`
(define-condition quux (buux)
  (x
   (y :initarg :y))
  (:documentation "quack quack")
  (:default-initargs :y 3))`, scope).Eval(scope, nil).(*clos.ConditionClass)

	tt.Equal(t, `(define-condition quux (buux)
  (x (y :initarg :y))
  (:documentation "quack quack")
  (:default-initargs :y 3))
`, string(pp.Append(nil, scope, quux.LoadForm())))

	tt.Equal(t, false, quux.Ready())
	tt.Panic(t, func() { _ = quux.MakeInstance() })

	buux := slip.ReadString(`
(define-condition buux ()
  ((z
    :initarg :z
    :initarg :zz
    :initform 3
    :reader get-z
    :writer set-z
    :accessor z-access
    :type fixnum)))`, scope).Eval(scope, nil).(*clos.ConditionClass)
	tt.Equal(t, true, buux.Ready())
	tt.Equal(t, true, quux.Ready())

	tt.Equal(t, "quack quack", quux.Documentation())
	tt.Equal(t, "", buux.Documentation())
	buux.SetDocumentation("buck buck")
	tt.Equal(t, "buck buck", buux.Documentation())

	tt.Equal(t, "#<condition-class quux>", quux.String())
	tt.Equal(t, false, quux.Equal(buux))

	tt.Equal(t, "[condition-class class standard-object t]", pretty.SEN(quux.Hierarchy()))
	tt.Equal(t, "quux", quux.Name())
	tt.Equal(t, true, quux == quux.Eval(nil, 0))

	tt.Equal(t, true, quux.Inherits(buux))
	tt.Equal(t, false, quux.Inherits(slip.FindClass("vanilla-flavor")))

	tt.Equal(t, slip.Symbol("condition-class"), quux.Metaclass())
}

func TestConditionClassAllocClass(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(makunbound 'quux)`, scope).Eval(scope, nil)
	quux := slip.ReadString(`
(define-condition quux ()
  ((x :initarg :x :initform 3 :allocation :class)))`, scope).Eval(scope, nil).(*clos.ConditionClass)

	desc := quux.Describe(nil, 0, 80, false)
	tt.Equal(t, `quux is a class:
  Direct superclasses:
  Class precedence list: quux condition t
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

func TestConditionClassMinimal(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(makunbound 'quux)`, scope).Eval(scope, nil)
	quux := slip.ReadString(`(define-condition quux () ())`, scope).Eval(scope, nil).(*clos.ConditionClass)
	desc := quux.Describe(nil, 0, 80, false)

	tt.Equal(t, `quux is a class:
  Direct superclasses:
  Class precedence list: quux condition t
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

func TestConditionClassMergeSupers(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(progn (makunbound 'quux) (makunbound 'buux) (makunbound 'duux))`, scope).Eval(scope, nil)
	quux := slip.ReadString(`(define-condition quux (buux duux) ())`, scope).Eval(scope, nil).(*clos.ConditionClass)
	_ = slip.ReadString(`(define-condition buux (duux) ())`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(define-condition duux () ())`, scope).Eval(scope, nil)

	tt.Equal(t, `(define-condition quux (buux duux)
  ())
`, string(pp.Append(nil, scope, quux.LoadForm())))

	tt.Equal(t, true, quux.Ready())
}
