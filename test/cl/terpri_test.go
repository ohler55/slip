// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/slip"
	"github.com/stretchr/testify/require"
)

func TestTerpriStream(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadString("(terpri out)").Eval(scope)
	require.Nil(t, result)
	require.Equal(t, "\n", out.String())
}

func TestTerpriStdout(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	orig := slip.StandardOutput
	defer func() { slip.StandardOutput = orig }()
	slip.StandardOutput = &slip.OutputStream{Writer: &out}
	result := slip.ReadString("(terpri)").Eval(scope)
	require.Nil(t, result)
	require.Equal(t, "\n", out.String())
}

func TestTerpriArgCount(t *testing.T) {
	require.Panics(t, func() { _ = slip.ReadString("(terpri nil nil)").Eval(slip.NewScope()) })
}

func TestTerpriBadStream(t *testing.T) {
	require.Panics(t, func() { _ = slip.ReadString("(terpri t)").Eval(slip.NewScope()) })
}

func TestTerpriWriteFail(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	require.Panics(t, func() { _ = slip.ReadString("(terpri out)").Eval(scope) })
}
