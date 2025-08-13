// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestTwoWayStreamObject(t *testing.T) {
	(&sliptest.Object{
		Target:    &cl.TwoWayStream{},
		String:    "#<TWO-WAY-STREAM>",
		Simple:    "#<TWO-WAY-STREAM>",
		Hierarchy: "two-way-stream.stream.t",
		Equals: []*sliptest.EqTest{
			{Other: &cl.TwoWayStream{}, Expect: true},
			{Other: slip.True, Expect: false},
		},
		Selfies: []func() slip.Symbol{
			(&cl.TwoWayStream{}).StreamType,
		},
		Eval: &cl.TwoWayStream{},
	}).Test(t)
	tt.Panic(t, func() { _ = (&cl.TwoWayStream{}).LoadForm() })
}

func TestTwoWayStreamReadWriteOk(t *testing.T) {
	var (
		ss1 slip.StringStream
		ss2 slip.StringStream
	)
	es := cl.NewTwoWayStream(&ss1, &ss2)
	scope := slip.NewScope()
	scope.Let("es", es)
	_, err := ss1.Write([]byte("abc"))
	tt.Nil(t, err)
	_, _ = ss1.Seek(0, 0)

	(&sliptest.Function{
		Scope:  scope,
		Source: "(read es)",
		Expect: "abc",
	}).Test(t)

	(&sliptest.Function{
		Scope:  scope,
		Source: "(princ 'def es)",
		Expect: "def",
	}).Test(t)
	tt.Equal(t, "def", ss2.Content())
}

func TestTwoWayStreamOpenClose(t *testing.T) {
	var (
		ss1 slip.StringStream
		ss2 slip.StringStream
	)
	es := cl.NewTwoWayStream(&ss1, &ss2)

	tt.Equal(t, true, es.IsOpen())
	_ = es.Close()
	tt.Equal(t, false, es.IsOpen())

	tt.Panic(t, func() { _, _ = es.Write([]byte{'x'}) })

	tt.Panic(t, func() {
		buf := make([]byte, 1)
		_, _ = es.Read(buf)
	})
}
