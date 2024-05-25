// Copyright (c) 2024, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestVarVal(t *testing.T) {
	vv := slip.UserPkg.Set("vv", slip.Fixnum(7))
	tt.Equal(t, `{doc: "" pkg: common-lisp-user val: 7}`, pretty.SEN(vv.Simplify()))
	tt.Equal(t, slip.Fixnum(7), vv.Value())
	tt.Equal(t, "vv", vv.String())
	tt.Equal(t, "vv", string(vv.Append(nil)))
	vv2 := vv
	tt.Equal(t, true, vv.Equal(vv2))
	tt.Equal(t, true, vv.Equal(slip.Symbol("vv")))
	tt.Equal(t, false, vv.Equal(slip.Symbol("zz")))
	tt.Equal(t, false, vv.Equal(slip.Fixnum(7)))
	tt.Equal(t, []slip.Symbol{slip.SymbolSymbol, slip.TrueSymbol}, vv.Hierarchy())
	tt.Equal(t, slip.Fixnum(7), vv.Eval(slip.NewScope(), 0))
}
