// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"errors"
	"io"
	"os"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestClose(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("file"), nil)
	_ = slip.ReadString(`(setq file (open "testdata/load-me.lisp"))`, scope).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(close file)`,
		Expect: "t",
	}).Test(t)
	result := slip.ReadString(`(close file)`, scope).Eval(scope, nil) // second close
	tt.Nil(t, result)
	f, _ := scope.Get(slip.Symbol("file")).(io.Closer)
	tt.NotNil(t, f)
	err := f.Close()
	tt.Equal(t, true, errors.Is(err, os.ErrClosed))
}

func TestCloseNotStream(t *testing.T) {
	(&sliptest.Function{
		Source: `(close t)`,
		Panics: true,
	}).Test(t)
}
