// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestAdjustArraySimple(t *testing.T) {
	(&sliptest.Function{
		Source: `(adjust-array (make-array '(1 2) :adjustable t) '(2 3))`,
		Validate: func(t *testing.T, v slip.Object) {
			a := v.(*slip.Array)
			tt.Equal(t, "((nil nil nil) (nil nil nil))", slip.ObjectString(a.AsList()))
			tt.Equal(t, true, a.Adjustable())
			tt.Equal(t, slip.TrueSymbol, a.ElementType())
		},
	}).Test(t)
}

func TestAdjustArrayVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(adjust-array (make-array '(4) :fill-pointer 3) 5)`,
		Validate: func(t *testing.T, v slip.Object) {
			vv := v.(*slip.Vector)
			tt.Equal(t, "(nil nil nil)", slip.ObjectString(vv.AsList()))
			tt.Equal(t, true, vv.Adjustable())
			tt.Equal(t, slip.TrueSymbol, vv.ElementType())
		},
	}).Test(t)
}

func TestAdjustArrayFillPointer(t *testing.T) {
	(&sliptest.Function{
		Source: `(adjust-array (make-array '(4)) 4 :fill-pointer nil)`,
		Validate: func(t *testing.T, v slip.Object) {
			vv := v.(*slip.Vector)
			tt.Equal(t, "#(nil nil nil nil)", slip.ObjectString(vv))
			tt.Equal(t, -1, vv.FillPtr)
		},
	}).Test(t)
	(&sliptest.Function{
		Source: `(adjust-array (make-array '(4)) 4 :fill-pointer t)`,
		Validate: func(t *testing.T, v slip.Object) {
			vv := v.(*slip.Vector)
			tt.Equal(t, "#(nil nil nil nil)", slip.ObjectString(vv))
			tt.Equal(t, 4, vv.FillPtr)
		},
	}).Test(t)
	(&sliptest.Function{
		Source: `(adjust-array (make-array '(4)) 4 :fill-pointer 2)`,
		Validate: func(t *testing.T, v slip.Object) {
			vv := v.(*slip.Vector)
			tt.Equal(t, "#(nil nil)", slip.ObjectString(vv))
			tt.Equal(t, 2, vv.FillPtr)
		},
	}).Test(t)
}

func TestAdjustArrayContents(t *testing.T) {
	(&sliptest.Function{
		Source: `(adjust-array (make-array '(1 2)) '(2 3) :initial-contents '((a b c)(d e f)))`,
		Validate: func(t *testing.T, v slip.Object) {
			a := v.(*slip.Array)
			tt.Equal(t, "((a b c) (d e f))", slip.ObjectString(a.AsList()))
		},
	}).Test(t)
}

func TestAdjustArrayElementType(t *testing.T) {
	(&sliptest.Function{
		Source: `(adjust-array (make-array 2 :adjustable nil) 4 :element-type 'fixnum :initial-element 1)`,
		Validate: func(t *testing.T, v slip.Object) {
			vv := v.(*slip.Vector)
			tt.Equal(t, "#(nil nil 1 1)", slip.ObjectString(vv))
			tt.Equal(t, false, vv.Adjustable())
		},
	}).Test(t)
	// (&sliptest.Function{
	// 	Source: `(adjust-array (make-array '(1 2) :adjustable nil) '(2 3) :element-type 'fixnum :initial-element 1)`,
	// 	Validate: func(t *testing.T, v slip.Object) {
	// 		a := v.(*slip.Array)
	// 		tt.Equal(t, "((nil nil 1) (1 1 1))", slip.ObjectString(a.AsList()))
	// 		tt.Equal(t, false, a.Adjustable())
	// 	},
	// }).Test(t)
}

func TestAdjustArrayNotArray(t *testing.T) {
	(&sliptest.Function{
		Source:    `(adjust-array t 5)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestAdjustArrayBadDims(t *testing.T) {
	(&sliptest.Function{
		Source:    `(adjust-array (make-array  2) t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(adjust-array (make-array '(1 2)) '(1 t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestAdjustArrayBadElementType(t *testing.T) {
	(&sliptest.Function{
		Source:    `(adjust-array (make-array '(1 2)) '(1 2) :element-type t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestAdjustArrayBadContents(t *testing.T) {
	(&sliptest.Function{
		Source:    `(adjust-array (make-array '(1 2)) '(1 2):initial-contents t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
