// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestFreshLineStream(t *testing.T) {
	var out slip.StringStream
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &out)
	result := slip.ReadString("(fresh-line out)", scope).Eval(scope, nil)
	tt.NotNil(t, result)
	tt.Equal(t, "\n", out.Content())

	result = slip.ReadString("(fresh-line out)", scope).Eval(scope, nil)
	tt.Nil(t, result)
	tt.Equal(t, "\n", out.Content())
}

func TestFreshLineStdout(t *testing.T) {
	var out slip.StringStream
	scope := slip.NewScope()

	orig := slip.StandardOutput
	defer func() { slip.StandardOutput = orig }()
	slip.StandardOutput = &slip.OutputStream{Writer: &out}
	result := slip.ReadString("(fresh-line)", scope).Eval(scope, nil)
	tt.NotNil(t, result)
	tt.Equal(t, "\n", out.Content())

	result = slip.ReadString("(fresh-line)", scope).Eval(scope, nil)
	tt.Nil(t, result)
	tt.Equal(t, "\n", out.Content())
}

func TestFreshLineBadStream(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadString("(fresh-line t)", scope).Eval(scope, nil) })
}

func TestFreshLineWriteFail(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	tt.Panic(t, func() { _ = slip.ReadString("(fresh-line out)", scope).Eval(scope, nil) })
}
