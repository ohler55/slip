// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestOpenStreamPOkay(t *testing.T) {
	var out strings.Builder
	stream := slip.OutputStream{Writer: &out}
	scope := slip.NewScope()
	scope.Let("out", &stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(open-stream-p out)`,
		Expect: "t",
	}).Test(t)
	_ = stream.Close()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(open-stream-p out)`,
		Expect: "nil",
	}).Test(t)
}

func TestOpenStreamPNotStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(open-stream-p t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
