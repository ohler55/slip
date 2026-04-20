// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank_test

import (
	"testing"

	"github.com/ohler55/slip"
	_ "github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestSwankVerboseNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: "(swank:swank-verbose)",
		Expect: "(:wire nil :dispatch nil :eval nil)",
	}).Test(t)
}

func TestSwankVerboseSetWire(t *testing.T) {
	(&sliptest.Function{
		Source: "(swank:swank-verbose :wire t)",
		Expect: "(:wire t :dispatch nil :eval nil)",
	}).Test(t)
	// Reset
	(&sliptest.Function{
		Source: "(swank:swank-verbose :wire nil :dispatch nil :eval nil)",
		Expect: "(:wire nil :dispatch nil :eval nil)",
	}).Test(t)
}

func TestSwankVerboseAllFlags(t *testing.T) {
	(&sliptest.Function{
		Source: "(swank:swank-verbose :wire t :dispatch t :eval t)",
		Expect: "(:wire t :dispatch t :eval t)",
	}).Test(t)
	// Reset
	(&sliptest.Function{
		Source: "(swank:swank-verbose :wire nil :dispatch nil :eval nil)",
		Expect: "(:wire nil :dispatch nil :eval nil)",
	}).Test(t)
}

func TestSwankVerboseInvalidKeyword(t *testing.T) {
	(&sliptest.Function{
		Source:    "(swank:swank-verbose :invalid t)",
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestSwankVerboseNonKeywordArg(t *testing.T) {
	(&sliptest.Function{
		Source:    "(swank:swank-verbose 42 t)",
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
