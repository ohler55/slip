// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMakeArraySimple(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-array '(2 3))`,
		Validate: func(t *testing.T, v slip.Object) {
			a := v.(*slip.Array)
			tt.Equal(t, "((nil nil nil) (nil nil nil))", slip.ObjectString(a.AsList()))
			tt.Equal(t, true, a.Adjustable())
			tt.Equal(t, slip.TrueSymbol, a.ElementType())
		},
	}).Test(t)
}

func TestMakeArrayVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-array '(4) :fill-pointer 3)`,
		Validate: func(t *testing.T, v slip.Object) {
			vv := v.(*slip.Vector)
			tt.Equal(t, "(nil nil nil)", slip.ObjectString(vv.AsList()))
			tt.Equal(t, true, vv.Adjustable())
			tt.Equal(t, slip.TrueSymbol, vv.ElementType())
		},
	}).Test(t)
}

func TestMakeArrayFillPointer(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-array '(4) :fill-pointer nil)`,
		Validate: func(t *testing.T, v slip.Object) {
			vv := v.(*slip.Vector)
			tt.Equal(t, "#(nil nil nil nil)", slip.ObjectString(vv))
			tt.Equal(t, -1, vv.FillPtr)
		},
	}).Test(t)
	(&sliptest.Function{
		Source: `(make-array '(4) :fill-pointer t)`,
		Validate: func(t *testing.T, v slip.Object) {
			vv := v.(*slip.Vector)
			tt.Equal(t, "#(nil nil nil nil)", slip.ObjectString(vv))
			tt.Equal(t, 4, vv.FillPtr)
		},
	}).Test(t)
}

func TestMakeArrayContents(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-array '(2 3) :initial-contents '((a b c)(d e f)))`,
		Validate: func(t *testing.T, v slip.Object) {
			a := v.(*slip.Array)
			tt.Equal(t, "((a b c) (d e f))", slip.ObjectString(a.AsList()))
		},
	}).Test(t)
}

func TestMakeArrayElementType(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-array '(2 3) :element-type 'fixnum :initial-element 1 :adjustable nil)`,
		Validate: func(t *testing.T, v slip.Object) {
			a := v.(*slip.Array)
			tt.Equal(t, "((1 1 1) (1 1 1))", slip.ObjectString(a.AsList()))
			tt.Equal(t, false, a.Adjustable())
		},
	}).Test(t)
}

func TestMakeArrayBadDims(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-array 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-array '(1 t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestMakeArrayBadElementType(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-array '(1 2) :element-type t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestMakeArrayBadContents(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-array '(1 2) :initial-contents t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
