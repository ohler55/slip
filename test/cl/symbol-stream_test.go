// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestSymbolStreamObject(t *testing.T) {
	(&sliptest.Object{
		Target:    cl.NewSymbolStream(slip.Symbol("ss")),
		String:    "#<SYMBOL-STREAM :symbol ss>",
		Simple:    "#<SYMBOL-STREAM :symbol ss>",
		Hierarchy: "symbol-stream.stream.t",
		Equals: []*sliptest.EqTest{
			{Other: cl.NewSymbolStream(slip.Symbol("ss")), Expect: true},
			{Other: slip.True, Expect: false},
		},
		Selfies: []func() slip.Symbol{
			(&cl.SymbolStream{}).StreamType,
		},
		Eval: cl.NewSymbolStream(slip.Symbol("ss")),
	}).Test(t)
}

func TestSymbolStreamReadWriteOk(t *testing.T) {
	var (
		ss1 slip.StringStream
		ss2 slip.StringStream
	)
	slip.CurrentPackage.Set("zz", &ss1)
	defer func() { _ = slip.CurrentPackage.Remove("zz") }()
	ss := cl.NewSymbolStream(slip.Symbol("zz"))
	_, err := ss1.Write([]byte("abc"))
	tt.Nil(t, err)
	_, _ = ss1.Seek(0, 0)
	scope := slip.NewScope()
	scope.Let("ss", ss)

	(&sliptest.Function{
		Scope:  scope,
		Source: "(read ss)",
		Expect: "abc",
	}).Test(t)

	slip.CurrentPackage.Set("zz", &ss2)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(princ 'def ss)",
		Expect: "def",
	}).Test(t)
	tt.Equal(t, "def", ss2.Content())
}

func TestSymbolStreamOpenClose(t *testing.T) {
	ss := cl.NewSymbolStream(slip.Symbol("zz"))
	slip.CurrentPackage.Set("zz", slip.NewStringStream([]byte("x")))
	defer func() { _ = slip.CurrentPackage.Remove("zz") }()

	tt.Equal(t, true, ss.IsOpen())
	ss.Close()
	tt.Equal(t, false, ss.IsOpen())

	tt.Panic(t, func() { _, _ = ss.Write([]byte{'x'}) })

	tt.Panic(t, func() {
		buf := make([]byte, 1)
		_, _ = ss.Read(buf)
	})
}

func TestSymbolStreamReadUnbound(t *testing.T) {
	ss := cl.NewSymbolStream(slip.Symbol("zz"))
	scope := slip.NewScope()
	scope.Let("ss", ss)

	(&sliptest.Function{
		Scope:     scope,
		Source:    "(read ss)",
		PanicType: slip.UnboundVariableSymbol,
	}).Test(t)
}

func TestSymbolStreamReadNotStream(t *testing.T) {
	ss := cl.NewSymbolStream(slip.Symbol("zz"))
	slip.CurrentPackage.Set("zz", slip.Fixnum(7))
	defer func() { _ = slip.CurrentPackage.Remove("zz") }()

	scope := slip.NewScope()
	scope.Let("ss", ss)

	(&sliptest.Function{
		Scope:     scope,
		Source:    "(read ss)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSymbolStreamWriteUnbound(t *testing.T) {
	ss := cl.NewSymbolStream(slip.Symbol("zz"))
	scope := slip.NewScope()
	scope.Let("ss", ss)

	(&sliptest.Function{
		Scope:     scope,
		Source:    "(princ 'x ss)",
		PanicType: slip.UnboundVariableSymbol,
	}).Test(t)
}

func TestSymbolStreamWriteNotStream(t *testing.T) {
	ss := cl.NewSymbolStream(slip.Symbol("zz"))
	slip.CurrentPackage.Set("zz", slip.Fixnum(7))
	defer func() { _ = slip.CurrentPackage.Remove("zz") }()

	scope := slip.NewScope()
	scope.Let("ss", ss)

	(&sliptest.Function{
		Scope:     scope,
		Source:    "(princ 'x ss)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
