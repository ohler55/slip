// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestPrettyPrintNil(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 6)) (pretty-print #(a b c d) nil))`,
		Expect: `"#(a b
  c d)
"`,
	}).Test(t)
}

func TestPrettyPrintStream(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 6))
                   (with-output-to-string (s)
                     (pretty-print #(a b c d) s)))`,
		Expect: `"#(a b
  c d)
"`,
	}).Test(t)
}

func TestPrettyPrintStdout(t *testing.T) {
	(&sliptest.Function{
		Source: `(with-output-to-string (s)
                   (let ((*print-right-margin* 6)
                         (*standard-output* s))
                     (pretty-print #(a b c d) t)))`,
		Expect: `"#(a b
  c d)
"`,
	}).Test(t)
}

func TestPrettyPrintBadDestination(t *testing.T) {
	(&sliptest.Function{
		Source:    `(pretty-print #(a b c d) 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestPrettyPrintWriteError(t *testing.T) {
	scope := slip.NewScope()
	slip.SetVar(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(pretty-print #(a b c d) out)`,
		PanicType: slip.StreamErrorSymbol,
	}).Test(t)
}
