// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSetBasic(t *testing.T) {
	qx := slip.NewFunc("quote", slip.List{slip.Symbol("x")})
	scope := slip.NewScope()
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("set", slip.List{slip.Fixnum(7), qx}),
		String: "(set 'x 7)",
		Simple: []interface{}{"set", []interface{}{"quote", "x"}, 7},
		Eval:   slip.Fixnum(7),
	}).Test(t)
	tt.Equal(t, slip.Fixnum(7), scope.Get(slip.Symbol("x")))
}

func TestSetBadArgCount(t *testing.T) {
	qx := slip.NewFunc("quote", slip.List{slip.Symbol("x")})
	scope := slip.NewScope()
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("set", slip.List{qx}),
		String: "(set 'x)",
		Simple: []interface{}{"set", []interface{}{"quote", "x"}},
		Panics: true,
	}).Test(t)
}

func TestSetBadSymbol(t *testing.T) {
	(&sliptest.Object{
		Target: slip.NewFunc("set", slip.List{slip.True, slip.True}),
		String: "(set t t)",
		Simple: []interface{}{"set", true, true},
		Panics: true,
	}).Test(t)
}
