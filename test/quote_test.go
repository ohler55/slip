// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestQuoteEmpty(t *testing.T) {
	(&sliptest.Object{
		Target: slip.NewFunc("quote", slip.List{nil}),
		String: "'nil",
		Simple: []interface{}{"quote", nil},
		Eval:   nil,
	}).Test(t)
}

func TestQuoteObject(t *testing.T) {
	(&sliptest.Object{
		Target: slip.NewFunc("quote", slip.List{slip.Symbol("abc")}),
		String: "'abc",
		Simple: []interface{}{"quote", "abc"},
		Eval:   slip.Symbol("abc"),
	}).Test(t)
}

func TestQuoteBadArgCount(t *testing.T) {
	(&sliptest.Object{
		Target: slip.NewFunc("quote", slip.List{}),
		String: "",
		Simple: []interface{}{"quote"},
		Panics: true,
	}).Test(t)
}
