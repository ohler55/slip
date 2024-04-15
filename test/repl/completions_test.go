// Copyright (c) 2024, Peter Ohler, All rights reserved.

package repl

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCompletionsOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(completions "princ")`,
		Expect: `("princ" "princ-to-string")`,
	}).Test(t)
}

func TestCompletionsEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(completions "quuxx")`,
		Expect: `nil`,
	}).Test(t)
}

func TestCompletionsBadPrefix(t *testing.T) {
	(&sliptest.Function{
		Source:    `(completions t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
