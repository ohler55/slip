// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestWarnSimple(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	orig := slip.ErrorOutput
	defer func() { slip.ErrorOutput = orig }()
	slip.ErrorOutput = &slip.OutputStream{Writer: &out}

	result := slip.ReadString(`(warn "numbers ~D and ~D" 2 3)`, scope).Eval(scope, nil)
	tt.Nil(t, result)
	tt.Equal(t, "Warning: numbers 2 and 3\n", out.String())
}

func TestWarnClass(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	orig := slip.ErrorOutput
	defer func() { slip.ErrorOutput = orig }()
	slip.ErrorOutput = &slip.OutputStream{Writer: &out}

	result := slip.ReadString(`(warn 'warning :message "danger")`, scope).Eval(scope, nil)
	tt.Nil(t, result)
	tt.Equal(t, "Warning: danger\n", out.String())
}

func TestWarnWriteFail(t *testing.T) {
	scope := slip.NewScope()
	orig := slip.ErrorOutput
	defer func() { slip.ErrorOutput = orig }()
	slip.ErrorOutput = &slip.OutputStream{Writer: badWriter(0)}

	tt.Panic(t, func() { _ = slip.ReadString(`(warn "fail")`, scope).Eval(scope, nil) })
}

func TestWarnBadDatum(t *testing.T) {
	(&sliptest.Function{
		Source:    "(warn t)",
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestWarnNotWarning(t *testing.T) {
	(&sliptest.Function{
		Source:    "(warn 'error)",
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
