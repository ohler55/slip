// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/ohler55/slip"
	"github.com/stretchr/testify/require"
)

func TestPrincStream(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadString("(princ 123 out)").Eval(scope)

	require.Equal(t, slip.Fixnum(123), result)
	require.Equal(t, "123", out.String())
}

func TestPrincStdout(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	orig := slip.StandardOutput
	defer func() { slip.StandardOutput = orig }()
	slip.StandardOutput = &slip.OutputStream{Writer: &out}
	result := slip.ReadString("(princ 123)").Eval(scope)

	require.Equal(t, slip.Fixnum(123), result)
	require.Equal(t, "123", out.String())
}

func TestPrincArgCount(t *testing.T) {
	require.Panics(t, func() { _ = slip.ReadString("(princ)").Eval(slip.NewScope()) })
}

func TestPrincBadStream(t *testing.T) {
	require.Panics(t, func() { _ = slip.ReadString("(princ 123 t)").Eval(slip.NewScope()) })
}

type badWriter int

func (w badWriter) Write([]byte) (int, error) {
	return 0, fmt.Errorf("oops")
}

func TestPrincWriteFail(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	require.Panics(t, func() { _ = slip.ReadString("(princ 123 out)").Eval(scope) })
}
