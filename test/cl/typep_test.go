// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestTypepTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(typep 7 'fixnum)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(typep nil 'null)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(typep '() 'null)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(typep '(1 2) 'list)`,
		Expect: "t",
	}).Test(t)
}

func TestTypepFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(typep t 'fixnum)`,
		Expect: "nil",
	}).Test(t)
}

func TestTypepBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(typep)`,
		Panics: true,
	}).Test(t)
}

func TestTypepNotSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(typep 7 t)`,
		Panics: true,
	}).Test(t)
}
