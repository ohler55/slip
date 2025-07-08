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
	sd := clos.NewSlotDef(slip.Symbol("quux"))

	tt.Equal(t, `{
  accessors: []
  allocation: instance
  docs: ""
  initform: null
  intargs: []
  name: quux
  readers: []
  type: ""
  writers: []
}`, pretty.SEN(sd.Simplify()))

	tt.Equal(t, slip.Symbol("quux"), sd.DefList())
}

func TestSlotDefOptions(t *testing.T) {
	scope := slip.NewScope()
	slotSpecs := slip.ReadString(`'((quux :initarg :quux
                                        :reader get-quux
                                        :writer set-quux
                                        :accessor quux
                                        :allocation :class
                                        :type fixnum
                                        :documentation "quack quack"
                                        :initform (1+ qqq)))`, scope).Eval(scope, nil).(slip.List)
	sc := clos.DefStandardClass("quux-class", nil, slotSpecs, nil)

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
`, string(pp.Append(nil, scope, sc.DefList())))
}

func TestSlotDefBadName(t *testing.T) {
	scope := slip.NewScope()
	def := slip.ReadString(`'("quux" :initarg :quux)`, scope).Eval(scope, nil)
	tt.Panic(t, func() { _ = clos.NewSlotDef(def) })
}

func TestSlotDefNotEven(t *testing.T) {
	scope := slip.NewScope()
	def := slip.ReadString(`'(quux :initarg)`, scope).Eval(scope, nil)
	tt.Panic(t, func() { _ = clos.NewSlotDef(def) })
}

func TestSlotDefNotAnOption(t *testing.T) {
	scope := slip.NewScope()
	def := slip.ReadString(`'(quux :not-an-option :x)`, scope).Eval(scope, nil)
	tt.Panic(t, func() { _ = clos.NewSlotDef(def) })
}

func TestSlotDefBadAllocation(t *testing.T) {
	scope := slip.NewScope()
	def := slip.ReadString(`'(quux :allocation :nothing)`, scope).Eval(scope, nil)
	tt.Panic(t, func() { _ = clos.NewSlotDef(def) })
}

func TestSlotDefDoubleInitform(t *testing.T) {
	scope := slip.NewScope()
	def := slip.ReadString(`'(quux :initform 5 :initform 6)`, scope).Eval(scope, nil)
	tt.Panic(t, func() { _ = clos.NewSlotDef(def) })
}

func TestSlotDefBadType(t *testing.T) {
	scope := slip.NewScope()
	def := slip.ReadString(`'(quux :type t)`, scope).Eval(scope, nil)
	tt.Panic(t, func() { _ = clos.NewSlotDef(def) })
}

func TestSlotDefDoubleType(t *testing.T) {
	scope := slip.NewScope()
	def := slip.ReadString(`'(quux :type fixnum :type float)`, scope).Eval(scope, nil)
	tt.Panic(t, func() { _ = clos.NewSlotDef(def) })
}

func TestSlotDefBadDoc(t *testing.T) {
	scope := slip.NewScope()
	def := slip.ReadString(`'(quux :documentation t)`, scope).Eval(scope, nil)
	tt.Panic(t, func() { _ = clos.NewSlotDef(def) })
}

func TestSlotDefDoubleDoc(t *testing.T) {
	scope := slip.NewScope()
	def := slip.ReadString(`'(quux :documentation "docs" :documentation "doc2")`, scope).Eval(scope, nil)
	tt.Panic(t, func() { _ = clos.NewSlotDef(def) })
}

func TestSlotDefBadSpec(t *testing.T) {
	tt.Panic(t, func() { _ = clos.NewSlotDef(slip.True) })
}

func TestSlotDefReaderNotSymbol(t *testing.T) {
	scope := slip.NewScope()
	def := slip.ReadString(`'(quux :reader t)`, scope).Eval(scope, nil)
	tt.Panic(t, func() { _ = clos.NewSlotDef(def) })
}
