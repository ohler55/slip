// Copyright (c) 2025, Peter Ohler, All rights reserved.

package test

import (
	"os"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFileStream(t *testing.T) {
	(&sliptest.Object{
		Target:    (*slip.FileStream)(os.Stdout),
		String:    "/^#<FILE-STREAM /dev/stdout \\{[0-9+]\\}>$/",
		Simple:    "#<FILE-STREAM /dev/stdout {1}>",
		Hierarchy: "file-stream.stream.t",
		Equals: []*sliptest.EqTest{
			{Other: (*slip.FileStream)(os.Stdout), Expect: true},
			{Other: slip.True, Expect: false},
		},
		Selfies: []func() slip.Symbol{
			(*slip.FileStream)(os.Stdout).StreamType,
		},
		Eval: (*slip.FileStream)(os.Stdout),
	}).Test(t)
}

func TestFileStreamFile(t *testing.T) {
	filename := "testdata/sample"
	defer func() { _ = os.Remove(filename) }()

	f, err := os.Create(filename)
	tt.Nil(t, err)
	fs := (*slip.FileStream)(f)
	tt.Equal(t, 0, fs.LastByte())
	_, err = f.WriteString("hello")
	tt.Nil(t, err)
	tt.Equal(t, 'o', fs.LastByte())
	tt.Equal(t, slip.Fixnum(5), fs.FileLength())
	pos, _ := fs.Seek(0, 1)
	tt.Equal(t, int64(5), pos)
}

func TestFileStreamWriteRead(t *testing.T) {
	pr, pw, err := os.Pipe()
	tt.Nil(t, err)
	defer func() { _ = pw.Close(); _ = pr.Close() }()

	lr := (*slip.FileStream)(pr)
	lw := (*slip.FileStream)(pw)

	_, _ = lw.Write([]byte("hello"))

	// Should not be able to get the last byte.
	tt.Equal(t, 0, lw.LastByte())

	tt.Equal(t, true, lw.IsOpen())
	_ = lw.Close()
	tt.Equal(t, false, lw.IsOpen())

	buf := make([]byte, 10)
	n, err := lr.Read(buf)
	tt.Nil(t, err)

	tt.Equal(t, "hello", string(buf[:n]))
}

func writeSampleFile(t *testing.T, filename string) *slip.FileStream {
	f, err := os.Create(filename)
	tt.Nil(t, err)
	_, err = f.WriteString("abc")
	tt.Nil(t, err)
	_, err = f.Seek(0, 0)
	tt.Nil(t, err)

	return (*slip.FileStream)(f)
}

func TestFileStreamReadChar(t *testing.T) {
	filename := "testdata/sample"
	defer func() { _ = os.Remove(filename) }()
	fs := writeSampleFile(t, filename)

	scope := slip.NewScope()
	scope.Let(slip.Symbol("in"), fs)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-char in)`,
		Expect: `#\a`,
	}).Test(t)
}

func TestFileStreamReadLine(t *testing.T) {
	filename := "testdata/sample"
	defer func() { _ = os.Remove(filename) }()
	fs := writeSampleFile(t, filename)

	scope := slip.NewScope()
	scope.Let(slip.Symbol("in"), fs)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-line in)`,
		Expect: `"abc", t`,
	}).Test(t)
}

func TestFileStreamPeekChar(t *testing.T) {
	filename := "testdata/sample"
	defer func() { _ = os.Remove(filename) }()
	fs := writeSampleFile(t, filename)

	scope := slip.NewScope()
	scope.Let(slip.Symbol("in"), fs)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(peek-char t in)`,
		PanicType: slip.StreamErrorSymbol,
	}).Test(t)
}

func TestFileStreamReadByte(t *testing.T) {
	filename := "testdata/sample"
	defer func() { _ = os.Remove(filename) }()
	fs := writeSampleFile(t, filename)

	scope := slip.NewScope()
	scope.Let(slip.Symbol("in"), fs)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-byte in)`,
		Expect: `97`,
	}).Test(t)
}
