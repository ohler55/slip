// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"os"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestConcatenatedStreamObject(t *testing.T) {
	var ss1 slip.StringStream
	(&sliptest.Object{
		Target:    cl.NewConcatenatedStream((*slip.FileStream)(os.Stdin)),
		String:    "#<CONCATENATED-STREAM>",
		Simple:    "#<CONCATENATED-STREAM>",
		Hierarchy: "concatenated-stream.stream.t",
		Equals: []*sliptest.EqTest{
			{Other: cl.NewConcatenatedStream((*slip.FileStream)(os.Stdin)), Expect: true},
			{Other: cl.NewConcatenatedStream(&ss1), Expect: false},
			{Other: slip.True, Expect: false},
		},
		Selfies: []func() slip.Symbol{
			cl.NewConcatenatedStream((*slip.FileStream)(os.Stdin)).StreamType,
		},
		Eval: cl.NewConcatenatedStream((*slip.FileStream)(os.Stdin)),
	}).Test(t)
}

func TestConcatenatedStreamReadOk(t *testing.T) {
	ss1 := slip.NewStringStream([]byte("abc"))
	ss2 := slip.NewStringStream([]byte("def"))

	cs := cl.NewConcatenatedStream(ss1, ss2)
	scope := slip.NewScope()
	scope.Let("cs", cs)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(read cs)",
		Expect: "abcdef",
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    "(read cs)",
		PanicType: slip.EndOfFileSymbol,
	}).Test(t)
}

func TestConcatenatedStreamReadSmall(t *testing.T) {
	ss1 := slip.NewStringStream([]byte("abc"))
	cs := cl.NewConcatenatedStream(ss1)
	buf := make([]byte, 2)
	n, err := cs.Read(buf)
	tt.Nil(t, err)
	tt.Equal(t, 2, n)
	tt.Equal(t, "ab", string(buf))
}

func TestConcatenatedStreamOpenClose(t *testing.T) {
	ss1 := slip.NewStringStream([]byte("abc"))
	ss2 := slip.NewStringStream([]byte("def"))

	cs := cl.NewConcatenatedStream(ss1, ss2)

	tt.Equal(t, true, cs.IsOpen())
	cs.Close()
	tt.Equal(t, false, cs.IsOpen())
	buf := []byte{'x'}
	_, err := cs.Read(buf)
	tt.NotNil(t, err)
}

func TestConcatenatedStreamNotStream(t *testing.T) {
	tt.Panic(t, func() { _ = cl.NewConcatenatedStream(slip.True) })
}

func TestConcatenatedStreamReadError(t *testing.T) {
	cs := cl.NewConcatenatedStream(&slip.InputStream{Reader: badReader(0)})
	buf := make([]byte, 2)
	_, err := cs.Read(buf)
	tt.NotNil(t, err)
}
