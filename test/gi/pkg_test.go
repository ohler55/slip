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

func TestGlobalStackTraceProvenance(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((orig *stack-trace-provenance*)
                       result)
                   (setq *stack-trace-provenance* nil)
                   (addf result *stack-trace-provenance*)
                   (setq *stack-trace-provenance* t)
                   (addf result *stack-trace-provenance*)
                   (setq *stack-trace-provenance* orig)
                   result)`,
		Expect: "(nil t)",
	}).Test(t)
}

func TestGlobalStackTraceFunction(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((orig *stack-trace-function*)
                       result)
                   (setq *stack-trace-function* nil)
                   (addf result *stack-trace-function*)
                   (setq *stack-trace-function* t)
                   (addf result *stack-trace-function*)
                   (setq *stack-trace-function* orig)
                   result)`,
		Expect: "(nil t)",
	}).Test(t)
}

func TestGlobalStackTraceFullFilenames(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((orig *stack-trace-full-filenames*)
                       result)
                   (setq *stack-trace-full-filenames* nil)
                   (addf result *stack-trace-full-filenames*)
                   (setq *stack-trace-full-filenames* t)
                   (addf result *stack-trace-full-filenames*)
                   (setq *stack-trace-full-filenames* orig)
                   result)`,
		Expect: "(nil t)",
	}).Test(t)
}
