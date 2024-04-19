// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestBooleBoole1(t *testing.T) {
	(&sliptest.Function{
		Source: `(boole boole-1 3 4)`,
		Expect: "3",
	}).Test(t)
}

func TestBooleBoole2(t *testing.T) {
	(&sliptest.Function{
		Source: `(boole boole-2 3 4)`,
		Expect: "4",
	}).Test(t)
}

func TestBooleBooleAnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(boole boole-and 5 3)`,
		Expect: "1",
	}).Test(t)
}

func TestBooleBooleAndC1(t *testing.T) {
	(&sliptest.Function{
		Source: `(boole boole-andc1 3 7)`,
		Expect: "4",
	}).Test(t)
}

func TestBooleBooleAndC2(t *testing.T) {
	(&sliptest.Function{
		Source: `(boole boole-andc2 7 3)`,
		Expect: "4",
	}).Test(t)
}

func TestBooleBooleC1(t *testing.T) {
	(&sliptest.Function{
		Source: `(boole boole-c1 7 0)`,
		Expect: "-8",
	}).Test(t)
	(&sliptest.Function{
		Source: `(boole boole-c1 -8 0)`,
		Expect: "7",
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~X" (boole boole-c1 (- 0 #x777777777777777777) 0))`,
		Expect: "7",
	}).Test(t)
}

// "boole-c1",
// "boole-c2",
// "boole-clr",
// "boole-eqv",
// "boole-ior",
// "boole-nand",
// "boole-nor",
// "boole-orc1",
// "boole-orc2",
// "boole-set",
// "boole-xor",

func TestBooleNotInteger(t *testing.T) {
	(&sliptest.Function{
		Source:    `(boole boole-and t 1)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(boole boole-and 1 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
