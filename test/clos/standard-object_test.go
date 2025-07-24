// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos_test

import (
	"bytes"
	"sort"
	"strings"
	"testing"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/clos"
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
	scope.Let("bub", nil)
	scope.Let("q2", nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(setq qube (make-instance 'quux :y 5))`,
		Expect: `/#<quux [0-9a-f]+>/`,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(setq q2 (make-instance 'quux :y 5))`,
		Expect: `/#<quux [0-9a-f]+>/`,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(setq bub (make-instance 'buux :z 3))`,
		Expect: `/#<buux [0-9a-f]+>/`,
	}).Test(t)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(list (slot-value qube 'x) (slot-value qube 'y) (slot-value qube 'z))`,
		Expect: `(nil 5 4)`,
	}).Test(t)

	obj := scope.Get("qube").(*clos.StandardObject)
	simple := obj.Simplify()
	_ = jp.C("id").Del(simple)
	tt.Equal(t, `{class: quux vars: {x: null y: 5 z: 4}}`, pretty.SEN(simple))

	tt.Equal(t, "/#<quux [0-9a-f]+>/", obj.String())
	tt.Equal(t, `[quux buux standard-object t]`, pretty.SEN(obj.Hierarchy()))

	tt.Equal(t, true, obj.IsA("quux"))
	tt.Equal(t, true, obj.IsA("buux"))
	tt.Equal(t, false, obj.IsA("vanilla-flavor"))

	obj.SetSlotValue(slip.Symbol("x"), slip.Fixnum(9))
	value, has := obj.SlotValue(slip.Symbol("x"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.Fixnum(9), value)

	tt.Equal(t, "quux", obj.Class().Name())

	bub := scope.Get("bub").(*clos.StandardObject)
	tt.Equal(t, false, obj.Equal(bub))

	tt.Equal(t, true, obj.Equal(obj.Eval(scope, 0)))
	q2 := scope.Get("q2").(*clos.StandardObject)
	tt.Equal(t, false, obj.Equal(q2))
	q2.SetSlotValue(slip.Symbol("x"), slip.Fixnum(9))
	tt.Equal(t, true, obj.Equal(q2))
	q2.SetSlotValue(slip.Symbol("x"), slip.Fixnum(6))
	tt.Equal(t, false, obj.Equal(q2))

	names := obj.SlotNames()
	sort.Strings(names)
	tt.Equal(t, `[x y z]`, pretty.SEN(names))
	tt.Equal(t, true, obj == obj.Eval(scope, 0))

	desc := obj.Describe(nil, 0, 80, false)
	tt.Equal(t, `/#<quux [0-9a-f]+>, an instance of class quux,
  has slot values:
    x: 9
    y: 5
    z: 4
/`, string(desc))

	desc = obj.Describe(nil, 0, 80, true)
	tt.Equal(t, true, bytes.Contains(desc, []byte{0x1b, '[', '1', 'm'}))

	tt.Panic(t, func() { _ = obj.Receive(scope, ":nothing", nil, 0) })
	namesStr := obj.MethodNames().String()
	tt.Equal(t, true, strings.Contains(namesStr, ":class"))
	tt.Equal(t, true, strings.Contains(namesStr, ":describe"))
	tt.Equal(t, true, strings.Contains(namesStr, ":print-self"))
	tt.NotNil(t, obj.GetMethod(":id"))
}

func TestStandardObjectInitform(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`
(defvar counter 3)
(defclass buux () ((z :initarg :z :initform (1+ counter))))
(defclass quux (buux)
  (x (y :initarg :y :initform 1)))`, scope).Eval(scope, nil)
	scope.Let("qube", nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(setq qube (make-instance 'quux))`,
		Expect: `/#<quux [0-9a-f]+>/`,
	}).Test(t)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(list (slot-value qube 'x) (slot-value qube 'y) (slot-value qube 'z))`,
		Expect: `(nil 1 4)`,
	}).Test(t)
}

func TestStandardObjectClassAlloc(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`
(defclass buux () ((b :initarg :b :allocation :class)))
(defclass quux (buux)
  ((q :initarg :q :initform 7 :allocation :class)))`, scope).Eval(scope, nil)
	scope.Let("qube", nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(setq qube (make-instance 'quux :b 52))`,
		Expect: `/#<quux [0-9a-f]+>/`,
	}).Test(t)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(list (slot-value qube 'b) (slot-value qube 'q))`,
		Expect: `(52 7)`,
	}).Test(t)

	obj := scope.Get("qube").(*clos.StandardObject)
	obj.SetSlotValue(slip.Symbol("b"), slip.Fixnum(9))
	value, has := obj.SlotValue(slip.Symbol("b"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.Fixnum(9), value)

	names := obj.SlotNames()
	sort.Strings(names)
	tt.Equal(t, `[b q]`, pretty.SEN(names))
}

func TestStandardObjectSlotType(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(defclass quux ()
                           ((x :initarg :x :type real)
                            (y :initarg :y :allocation :class :type :real)))`, scope).Eval(scope, nil)
	(&sliptest.Function{
		Source: `(slot-value (make-instance 'quux :x 7) 'x)`,
		Expect: `7`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(slot-value (make-instance 'quux :x 1.5) 'x)`,
		Expect: `1.5`,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(slot-value (make-instance 'quux :x "bad") 'x)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestStandardObjectDefaultInits(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(defclass quux ()
                           ((x :initarg :x :initarg :xx)
                            (y :initarg :y))
                           (:default-initargs :x nil :y 3))`, scope).Eval(scope, nil)
	(&sliptest.Function{
		Source: `(slot-value (make-instance 'quux) 'x)`,
		Expect: `nil`,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(slot-value (make-instance 'quux :z 1) 'x)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(slot-value (make-instance 'quux :x 1 :xx 2) 'x)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
