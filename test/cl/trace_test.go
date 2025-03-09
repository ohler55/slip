// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTraceAll(t *testing.T) {
	(&sliptest.Function{
		Source: `(trace t)(untrace)`,
		Expect: "nil",
	}).Test(t)
}

func TestTraceNames(t *testing.T) {
	scope := slip.NewScope()
	defer func() { _ = slip.ReadString("(untrace)", scope).Eval(scope, nil) }()
	(&sliptest.Function{
		Source: `(progn (trace car cdr) (trace))`,
		Expect: "(car cdr)",
	}).Test(t)
}

func TestTraceBadArgs(t *testing.T) {
	(&sliptest.Function{
		Source:    `(trace 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(trace car 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
