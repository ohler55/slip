// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestGlobalProvenance(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((orig *provenance*)
                       result)
                   (setq *provenance* nil)
                   (addf result *provenance*)
                   (setq *provenance* t)
                   (addf result *provenance*)
                   (setq *provenance* orig)
                   result)`,
		Expect: "(nil t)",
	}).Test(t)
}

func TestGlobalCoverage(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((orig *coverage*)
                       result)
                   (setq *coverage* nil)
                   (addf result *coverage*)
                   (setq *coverage* t)
                   (addf result *coverage*)
                   (setq *coverage* orig)
                   result)`,
		Expect: "(nil t)",
	}).Test(t)
}
