// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/clos"
	"github.com/ohler55/slip/pp"
)

func TestSlotDefSymbol(t *testing.T) {
	sd := clos.NewSlotDef(slip.NewScope(), slip.Symbol("quux"), 0)

	tt.Equal(t, `{
  accessors: []
  allocation: instance
  docs: ""
  initform: null
  intargs: []
  name: quux
  readers: []
  type: null
  writers: []
}`, pretty.SEN(sd.Simplify()))

	tt.Equal(t, slip.Symbol("quux"), sd.LoadForm())
}

func TestSlotDefOptions(t *testing.T) {
	scope := slip.NewScope()
	slotSpecs := slip.ReadEvalString(`'((quux :initarg :quux
                                        :reader get-quux
                                        :writer set-quux
                                        :accessor quux
                                        :allocation :class
                                        :type fixnum
                                        :documentation "quack quack"
                                        :initform (1+ qqq)))`, scope).(slip.List)
	sc := clos.DefStandardClass(scope, "quux-class", nil, slotSpecs, nil, 0)

	simple := sc.Simplify()
	tt.Equal(t, `{
  quux: {
    accessors: [quux]
    allocation: class
    docs: "quack quack"
    initform: ["1+" qqq]
    intargs: [":quux"]
    name: quux
    readers: [get-quux]
    type: fixnum
    writers: [set-quux]
  }
}`, pretty.SEN(jp.C("slotDefs").First(simple)))

	tt.Equal(t, `(defclass quux-class ()
  ((quux
    :initarg :quux
    :readers get-quux
    :writers set-quux
    :accessors quux
    :documentation "quack quack"
    :allocation :class
    :initform (1+ qqq)
    :type fixnum)))
`, string(pp.Append(nil, scope, sc.LoadForm())))

	tt.Equal(t, `quux-class is a class:
  Direct superclasses:
  Class precedence list: quux-class standard-object t
  Slots:
    quux
      initargs: :quux
      initform: (1+ qqq)
      readers: get-quux
      writers: set-quux
      accessors: quux
      documentation: quack quack
      allocation: class
      type: fixnum
  Class Slots:
    quux = (1+ qqq)
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
`, string(sc.Describe(nil, 0, 80, false)))
}

func TestSlotDefBadName(t *testing.T) {
	scope := slip.NewScope()
	def := slip.ReadEvalString(`'("quux" :initarg :quux)`, scope)
	tt.Panic(t, func() { _ = clos.NewSlotDef(scope, def, 0) })
}

func TestSlotDefNotEven(t *testing.T) {
	scope := slip.NewScope()
	def := slip.ReadEvalString(`'(quux :initarg)`, scope)
	tt.Panic(t, func() { _ = clos.NewSlotDef(scope, def, 0) })
}

func TestSlotDefNotAnOption(t *testing.T) {
	scope := slip.NewScope()
	def := slip.ReadEvalString(`'(quux :not-an-option :x)`, scope)
	tt.Panic(t, func() { _ = clos.NewSlotDef(scope, def, 0) })
}

func TestSlotDefBadAllocation(t *testing.T) {
	scope := slip.NewScope()
	def := slip.ReadEvalString(`'(quux :allocation :nothing)`, scope)
	tt.Panic(t, func() { _ = clos.NewSlotDef(scope, def, 0) })
}

func TestSlotDefDoubleInitform(t *testing.T) {
	scope := slip.NewScope()
	def := slip.ReadEvalString(`'(quux :initform 5 :initform 6)`, scope)
	tt.Panic(t, func() { _ = clos.NewSlotDef(scope, def, 0) })
}

func TestSlotDefBadType(t *testing.T) {
	scope := slip.NewScope()
	def := slip.ReadEvalString(`'(quux :type t)`, scope)
	tt.Panic(t, func() { _ = clos.NewSlotDef(scope, def, 0) })
}

func TestSlotDefDoubleType(t *testing.T) {
	scope := slip.NewScope()
	def := slip.ReadEvalString(`'(quux :type fixnum :type float)`, scope)
	tt.Panic(t, func() { _ = clos.NewSlotDef(scope, def, 0) })
}

func TestSlotDefBadDoc(t *testing.T) {
	scope := slip.NewScope()
	def := slip.ReadEvalString(`'(quux :documentation t)`, scope)
	tt.Panic(t, func() { _ = clos.NewSlotDef(scope, def, 0) })
}

func TestSlotDefDoubleDoc(t *testing.T) {
	scope := slip.NewScope()
	def := slip.ReadEvalString(`'(quux :documentation "docs" :documentation "doc2")`, scope)
	tt.Panic(t, func() { _ = clos.NewSlotDef(scope, def, 0) })
}

func TestSlotDefBadSpec(t *testing.T) {
	tt.Panic(t, func() { _ = clos.NewSlotDef(slip.NewScope(), slip.True, 0) })
}

func TestSlotDefReaderNotSymbol(t *testing.T) {
	scope := slip.NewScope()
	def := slip.ReadEvalString(`'(quux :reader t)`, scope)
	tt.Panic(t, func() { _ = clos.NewSlotDef(scope, def, 0) })
}
