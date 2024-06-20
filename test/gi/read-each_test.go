// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestReadEachOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let (quux)
                  (read-each (make-string-input-stream "(1 2 3)") (lambda (x) (setq quux x)))
                  quux)`,
		Expect: "(1 2 3)",
	}).Test(t)
}

func TestReadEachNotStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(read-each t (lambda (x) (setq quux x)))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
