// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"os"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDribbleNormal(t *testing.T) {
	origIn := slip.CurrentPackage.JustGet("*standard-input*")
	origOut := slip.CurrentPackage.JustGet("*standard-output*")
	defer func() {
		_ = slip.CurrentPackage.Set("*standard-input*", origIn)
		_ = slip.CurrentPackage.Set("*standard-output*", origOut)
		_ = os.Remove("testdata/dribble")
	}()

	var (
		in  slip.StringStream
		out slip.StringStream
	)
	_ = slip.CurrentPackage.Set("*standard-input*", &in)
	_ = slip.CurrentPackage.Set("*standard-output*", &out)
	_, err := in.Write([]byte("abc"))
	tt.Nil(t, err)
	_, _ = in.Seek(0, 0)

	(&sliptest.Function{
		Source: `(let (input)
                  (dribble "testdata/dribble")
                  (setq input (read))
                  (princ "def")
                  (dribble)
                  (princ "ghi")
                  input)`,
		Expect: "abc",
	}).Test(t)
	tt.Equal(t, "defghi", out.Content())

	var content []byte
	content, err = os.ReadFile("testdata/dribble")
	tt.Nil(t, err)
	tt.Equal(t, `< a
< b
< c
> def
`, string(content))
}

func TestDribbleDouble(t *testing.T) {
	origIn := slip.CurrentPackage.JustGet("*standard-input*")
	origOut := slip.CurrentPackage.JustGet("*standard-output*")
	defer func() {
		_ = slip.CurrentPackage.Set("*standard-input*", origIn)
		_ = slip.CurrentPackage.Set("*standard-output*", origOut)
		_ = os.Remove("testdata/dribble")
		_ = os.Remove("testdata/dribble2")
	}()

	var (
		in  slip.StringStream
		out slip.StringStream
	)
	_ = slip.CurrentPackage.Set("*standard-input*", &in)
	_ = slip.CurrentPackage.Set("*standard-output*", &out)

	(&sliptest.Function{
		Source: `(progn
                  (dribble "testdata/dribble")
                  (princ "abc")
                  (dribble "testdata/dribble2")
                  (princ "def")
                  (dribble))`,
		Expect: "nil",
	}).Test(t)

	content, err := os.ReadFile("testdata/dribble")
	tt.Nil(t, err)
	tt.Equal(t, "> abc\n", string(content))
	content, err = os.ReadFile("testdata/dribble2")
	tt.Nil(t, err)
	tt.Equal(t, "> def\n", string(content))
}

func TestDribbleBadFilepath(t *testing.T) {
	(&sliptest.Function{
		Source:    `(dribble t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDribbleFileError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(dribble "testdata")`,
		PanicType: slip.FileErrorSymbol,
	}).Test(t)
}
