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

func TestBroadcastStreamObject(t *testing.T) {
	var ss1 slip.StringStream
	(&sliptest.Object{
		Target:    cl.BroadcastStream{(*slip.FileStream)(os.Stdout)},
		String:    "#<BROADCAST-STREAM>",
		Simple:    "#<BROADCAST-STREAM>",
		Hierarchy: "broadcast-stream.stream.t",
		Equals: []*sliptest.EqTest{
			{Other: cl.BroadcastStream{(*slip.FileStream)(os.Stdout)}, Expect: true},
			{Other: cl.BroadcastStream{&ss1}, Expect: false},
			{Other: slip.True, Expect: false},
		},
		Selfies: []func() slip.Symbol{
			cl.BroadcastStream{(*slip.FileStream)(os.Stdout)}.StreamType,
		},
		Eval: cl.BroadcastStream{(*slip.FileStream)(os.Stdout)},
	}).Test(t)
}

func TestBroadcastStreamWriteOk(t *testing.T) {
	var (
		ss1 slip.StringStream
		ss2 slip.StringStream
	)
	bs := cl.BroadcastStream{&ss1, &ss2}
	scope := slip.NewScope()
	scope.Let("bs", bs)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(princ 'hello bs)",
		Expect: "hello",
	}).Test(t)
	tt.Equal(t, "hello", ss1.Content())
	tt.Equal(t, "hello", ss2.Content())
	tt.Equal(t, 'o', bs.LastByte())
	n, err := bs.Seek(0, 1)
	tt.Nil(t, err)
	tt.Equal(t, 5, n)
}

func TestBroadcastStreamWriteFail(t *testing.T) {
	ss1 := slip.OutputStream{Writer: badWriter(1)}
	var ss2 slip.StringStream
	bs := cl.BroadcastStream{&ss1, &ss2}
	scope := slip.NewScope()
	scope.Let("bs", bs)
	(&sliptest.Function{
		Scope:     scope,
		Source:    "(princ 'hello bs)",
		PanicType: slip.StreamErrorSymbol,
	}).Test(t)
	tt.Equal(t, "", ss2.Content())
}

func TestBroadcastStreamOpenClose(t *testing.T) {
	var (
		ss1 slip.StringStream
		ss2 slip.StringStream
	)
	bs := cl.BroadcastStream{&ss1, &ss2}

	tt.Equal(t, true, bs.IsOpen())
	bs.Close()
	tt.Equal(t, false, bs.IsOpen())
}

// TBD FileLength

func TestBroadcastStreamFileLength(t *testing.T) {
	filename := "testdata/sample1"
	defer func() { _ = os.Remove(filename) }()

	f, err := os.Create(filename)
	tt.Nil(t, err)
	fs := (*slip.FileStream)(f)
	var ss1 slip.StringStream

	bs := cl.BroadcastStream{&ss1, fs}

	n, err := bs.Write([]byte("hello"))
	tt.Nil(t, err)
	tt.Equal(t, 5, n)

	tt.Equal(t, slip.Fixnum(5), bs.FileLength())
}
