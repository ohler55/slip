// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestBackquoteEmpty(t *testing.T) {
	(&sliptest.Object{
		Target: slip.NewFunc("backquote", slip.List{nil}),
		String: "`nil",
		Simple: []interface{}{"backquote", nil},
		Eval:   nil,
	}).Test(t)
}

func TestBackquoteObject(t *testing.T) {
	(&sliptest.Object{
		Target: slip.NewFunc("backquote", slip.List{slip.Symbol("abc")}),
		String: "`abc",
		Simple: []interface{}{"backquote", "abc"},
		Eval:   slip.Symbol("abc"),
	}).Test(t)
}

func TestBackquoteBadArgCount(t *testing.T) {
	(&sliptest.Object{
		Target: slip.NewFunc("backquote", slip.List{}),
		String: "",
		Simple: []interface{}{"backquote"},
		Panics: true,
	}).Test(t)
}

func TestBackquoteSpecialCount(t *testing.T) {
	var b cl.Backquote

	tt.Equal(t, '`', b.SpecialChar())
}
