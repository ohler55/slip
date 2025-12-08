// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestEchoStreamObject(t *testing.T) {
	(&sliptest.Object{
		Target:    &cl.EchoStream{},
		String:    "#<ECHO-STREAM>",
		Simple:    "#<ECHO-STREAM>",
		Hierarchy: "echo-stream.stream.t",
		Equals: []*sliptest.EqTest{
			{Other: &cl.EchoStream{}, Expect: true},
			{Other: slip.True, Expect: false},
		},
		Selfies: []func() slip.Symbol{
			(&cl.EchoStream{}).StreamType,
		},
		Eval: &cl.EchoStream{},
	}).Test(t)
}

func TestEchoStreamReadWriteOk(t *testing.T) {
	var (
		ss1 slip.StringStream
		ss2 slip.StringStream
	)
	es := cl.NewEchoStream(&ss1, &ss2)
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
	tt.Equal(t, "abc", ss2.Content())

	(&sliptest.Function{
		Scope:  scope,
		Source: "(princ 'def es)",
		Expect: "def",
	}).Test(t)
	tt.Equal(t, "abc", ss1.Content())
	tt.Equal(t, "abcdef", ss2.Content())
}

func TestEchoStreamOpenClose(t *testing.T) {
	var (
		ss1 slip.StringStream
		ss2 slip.StringStream
	)
	es := cl.NewEchoStream(&ss1, &ss2)

	tt.Equal(t, true, es.IsOpen())
	_ = es.Close()
	tt.Equal(t, false, es.IsOpen())

	tt.Panic(t, func() { _, _ = es.Write([]byte{'x'}) })

	tt.Panic(t, func() {
		buf := make([]byte, 1)
		_, _ = es.Read(buf)
	})

	tt.Panic(t, func() { _, _ = es.ReadByte() })
	tt.Panic(t, func() { _, _, _ = es.ReadRune() })
	tt.Panic(t, func() { _ = es.UnreadRune() })
}

func TestEchoStreamReadChar(t *testing.T) {
	in := slip.NewStringStream([]byte("abc"))
	out := slip.NewStringStream([]byte{})
	es := cl.NewEchoStream(in, out)

	scope := slip.NewScope()
	scope.Let(slip.Symbol("in"), es)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-char in)`,
		Expect: `#\a`,
	}).Test(t)
}

func TestEchoStreamReadLine(t *testing.T) {
	in := slip.NewStringStream([]byte("abc\n"))
	out := slip.NewStringStream([]byte{})
	es := cl.NewEchoStream(in, out)

	scope := slip.NewScope()
	scope.Let(slip.Symbol("in"), es)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-line in)`,
		Expect: `"abc", nil`,
	}).Test(t)
}

func TestEchoStreamPeekChar(t *testing.T) {
	in := slip.NewStringStream([]byte("abc"))
	out := slip.NewStringStream([]byte{})
	es := cl.NewEchoStream(in, out)

	scope := slip.NewScope()
	scope.Let(slip.Symbol("in"), es)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(list (peek-char t in) (read-char in))`,
		Expect: `(#\a #\a)`,
	}).Test(t)
}

func TestEchoStreamReadByte(t *testing.T) {
	in := slip.NewStringStream([]byte("abc"))
	out := slip.NewStringStream([]byte{})
	es := cl.NewEchoStream(in, out)

	scope := slip.NewScope()
	scope.Let(slip.Symbol("in"), es)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-byte in)`,
		Expect: `97`,
	}).Test(t)
}

func TestEchoStreamUnreadRune(t *testing.T) {
	in := slip.NewStringStream([]byte("abc"))
	out := slip.NewStringStream([]byte{})
	es := cl.NewEchoStream(in, out)

	r, size, err := es.ReadRune()
	tt.Nil(t, err)
	tt.Equal(t, 1, size)
	tt.Equal(t, rune('a'), r)

	err = es.UnreadRune()
	tt.Nil(t, err)

	r, size, err = es.ReadRune()
	tt.Nil(t, err)
	tt.Equal(t, 1, size)
	tt.Equal(t, rune('a'), r)
}
