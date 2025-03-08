// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestTerpriStream(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadString("(terpri out)", scope).Eval(scope, nil)
	tt.Nil(t, result)
	tt.Equal(t, "\n", out.String())
}

func TestTerpriStdout(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	orig := slip.StandardOutput
	defer func() { slip.StandardOutput = orig }()
	slip.StandardOutput = &slip.OutputStream{Writer: &out}
	result := slip.ReadString("(terpri)", scope).Eval(scope, nil)
	tt.Nil(t, result)
	tt.Equal(t, "\n", out.String())
}

func TestTerpriArgCount(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadString("(terpri nil nil)", scope).Eval(scope, nil) })
}

func TestTerpriBadStream(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadString("(terpri t)", scope).Eval(scope, nil) })
}

func TestTerpriWriteFail(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	tt.Panic(t, func() { _ = slip.ReadString("(terpri out)", scope).Eval(scope, nil) })
}
