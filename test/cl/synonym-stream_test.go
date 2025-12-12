// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestSynonymStreamObject(t *testing.T) {
	(&sliptest.Object{
		Target:    cl.NewSynonymStream(slip.Symbol("ss")),
		String:    "#<SYNONYM-STREAM :symbol ss>",
		Simple:    "#<SYNONYM-STREAM :symbol ss>",
		Hierarchy: "synonym-stream.stream.t",
		Equals: []*sliptest.EqTest{
			{Other: cl.NewSynonymStream(slip.Symbol("ss")), Expect: true},
			{Other: slip.True, Expect: false},
		},
		Selfies: []func() slip.Symbol{
			(&cl.SynonymStream{}).StreamType,
		},
		Eval: cl.NewSynonymStream(slip.Symbol("ss")),
	}).Test(t)
}

func TestSynonymStreamReadWriteOk(t *testing.T) {
	var (
		ss1 slip.StringStream
		ss2 slip.StringStream
	)
	slip.CurrentPackage.Set("zz", &ss1)
	defer func() { _ = slip.CurrentPackage.Remove("zz") }()
	ss := cl.NewSynonymStream(slip.Symbol("zz"))
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

func TestSynonymStreamReadWriteStandard(t *testing.T) {
	var (
		ss1 slip.StringStream
		ss2 slip.StringStream
	)
	ss := cl.NewSynonymStream(slip.Symbol("zz"))
	_, err := ss1.Write([]byte("abc"))
	tt.Nil(t, err)
	_, _ = ss1.Seek(0, 0)

	stdin := slip.CurrentPackage.JustGet("*standard-input*")
	stdout := slip.CurrentPackage.JustGet("*standard-output*")
	slip.CurrentPackage.Set("*standard-input*", &ss1)
	slip.CurrentPackage.Set("*standard-output*", &ss2)
	defer func() {
		slip.CurrentPackage.Set("*standard-input*", stdin)
		slip.CurrentPackage.Set("*standard-output*", stdout)
		_ = slip.CurrentPackage.Remove("zz")
	}()

	scope := slip.NewScope()
	scope.Let("ss", ss)

	slip.CurrentPackage.Set("zz", nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(read ss)",
		Expect: "abc",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(princ 'def ss)",
		Expect: "def",
	}).Test(t)
	tt.Equal(t, "def", ss2.Content())

	slip.CurrentPackage.Set("zz", slip.True)
	_, _ = ss1.Seek(0, 0)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(read ss)",
		Expect: "abc",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(princ 'def ss)",
		Expect: "def",
	}).Test(t)
	tt.Equal(t, "defdef", ss2.Content())
}

func TestSynonymStreamOpenClose(t *testing.T) {
	ss := cl.NewSynonymStream(slip.Symbol("zz"))
	slip.CurrentPackage.Set("zz", slip.NewStringStream([]byte("x")))
	defer func() { _ = slip.CurrentPackage.Remove("zz") }()

	tt.Equal(t, true, ss.IsOpen())
	_ = ss.Close()
	tt.Equal(t, false, ss.IsOpen())

	tt.Panic(t, func() { _, _ = ss.Write([]byte{'x'}) })

	tt.Panic(t, func() {
		buf := make([]byte, 1)
		_, _ = ss.Read(buf)
	})
}

func TestSynonymStreamReadUnbound(t *testing.T) {
	ss := cl.NewSynonymStream(slip.Symbol("zz"))
	scope := slip.NewScope()
	scope.Let("ss", ss)

	(&sliptest.Function{
		Scope:     scope,
		Source:    "(read ss)",
		PanicType: slip.UnboundVariableSymbol,
	}).Test(t)
}

func TestSynonymStreamReadNotStream(t *testing.T) {
	ss := cl.NewSynonymStream(slip.Symbol("zz"))
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

func TestSynonymStreamWriteUnbound(t *testing.T) {
	ss := cl.NewSynonymStream(slip.Symbol("zz"))
	scope := slip.NewScope()
	scope.Let("ss", ss)

	(&sliptest.Function{
		Scope:     scope,
		Source:    "(princ 'x ss)",
		PanicType: slip.UnboundVariableSymbol,
	}).Test(t)
}

func TestSynonymStreamWriteNotStream(t *testing.T) {
	ss := cl.NewSynonymStream(slip.Symbol("zz"))
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

func TestSynonymStreamReadChar(t *testing.T) {
	ss := cl.NewSynonymStream(slip.Symbol("zz"))
	slip.CurrentPackage.Set("zz", slip.NewStringStream([]byte("abc")))
	defer func() { _ = slip.CurrentPackage.Remove("zz") }()

	scope := slip.NewScope()
	scope.Let(slip.Symbol("in"), ss)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-char in)`,
		Expect: `#\a`,
	}).Test(t)
}

func TestSynonymStreamReadLine(t *testing.T) {
	ss := cl.NewSynonymStream(slip.Symbol("zz"))
	slip.CurrentPackage.Set("zz", slip.NewStringStream([]byte("abc")))
	defer func() { _ = slip.CurrentPackage.Remove("zz") }()

	scope := slip.NewScope()
	scope.Let(slip.Symbol("in"), ss)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-line in)`,
		Expect: `"abc", t`,
	}).Test(t)
}

func TestSynonymStreamPeekChar(t *testing.T) {
	ss := cl.NewSynonymStream(slip.Symbol("zz"))
	slip.CurrentPackage.Set("zz", slip.NewStringStream([]byte("abc")))
	defer func() { _ = slip.CurrentPackage.Remove("zz") }()

	scope := slip.NewScope()
	scope.Let(slip.Symbol("in"), ss)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(peek-char t in)`,
		PanicType: slip.StreamErrorSymbol,
	}).Test(t)
}

func TestSynonymStreamReadByte(t *testing.T) {
	ss := cl.NewSynonymStream(slip.Symbol("zz"))
	slip.CurrentPackage.Set("zz", slip.NewStringStream([]byte("abc")))
	defer func() { _ = slip.CurrentPackage.Remove("zz") }()

	scope := slip.NewScope()
	scope.Let(slip.Symbol("in"), ss)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-byte in)`,
		Expect: `97`,
	}).Test(t)
}
