// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestBoole1(t *testing.T) {
	(&sliptest.Function{
		Source: `(boole boole-1 3 4)`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(boole boole-1 t 4)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestBoole2(t *testing.T) {
	(&sliptest.Function{
		Source: `(boole boole-2 3 4)`,
		Expect: "4",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(boole boole-2 4 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestBooleAnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(boole boole-and 5 3)`,
		Expect: "1",
	}).Test(t)
}

func TestBooleAndC1(t *testing.T) {
	(&sliptest.Function{
		Source: `(boole boole-andc1 3 7)`,
		Expect: "4",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(boole boole-andc1 t 4)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestBooleAndC2(t *testing.T) {
	(&sliptest.Function{
		Source: `(boole boole-andc2 7 3)`,
		Expect: "4",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(boole boole-andc2 4 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestBooleC1(t *testing.T) {
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
		Expect: `"777777777777777776"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~X" (boole boole-c1 #x777777777777777777 0))`,
		Expect: `"-777777777777777778"`,
	}).Test(t)
}

func TestBooleC2(t *testing.T) {
	(&sliptest.Function{
		Source: `(boole boole-c2 0 7)`,
		Expect: "-8",
	}).Test(t)
	(&sliptest.Function{
		Source: `(boole boole-c2 0 -8)`,
		Expect: "7",
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~X" (boole boole-c2 0 (- 0 #x777777777777777777)))`,
		Expect: `"777777777777777776"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~X" (boole boole-c2 0 #x777777777777777777))`,
		Expect: `"-777777777777777778"`,
	}).Test(t)
}

func TestBooleClr(t *testing.T) {
	(&sliptest.Function{
		Source: `(boole boole-clr 0 7)`,
		Expect: "0",
	}).Test(t)
}

func TestBooleEqv(t *testing.T) {
	(&sliptest.Function{
		Source: `(boole boole-eqv 7 3)`,
		Expect: "-5",
	}).Test(t)
}

func TestBooleIor(t *testing.T) {
	(&sliptest.Function{
		Source: `(boole boole-ior 4 2)`,
		Expect: "6",
	}).Test(t)
}

func TestBooleNand(t *testing.T) {
	(&sliptest.Function{
		Source: `(boole boole-nand 7 3)`,
		Expect: "-4",
	}).Test(t)
}

func TestBooleNor(t *testing.T) {
	(&sliptest.Function{
		Source: `(boole boole-nor 7 3)`,
		Expect: "-8",
	}).Test(t)
}

func TestBooleOrc1(t *testing.T) {
	(&sliptest.Function{
		Source: `(boole boole-orc1 7 3)`,
		Expect: "-5",
	}).Test(t)
}

func TestBooleOrc2(t *testing.T) {
	(&sliptest.Function{
		Source: `(boole boole-orc2 3 7)`,
		Expect: "-5",
	}).Test(t)
}

func TestBooleSet(t *testing.T) {
	(&sliptest.Function{
		Source: `(boole boole-set 0 7)`,
		Expect: "-1",
	}).Test(t)
}

func TestBooleXor(t *testing.T) {
	(&sliptest.Function{
		Source: `(boole boole-xor 4 7)`,
		Expect: "3",
	}).Test(t)
}

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

func TestBooleBadOp(t *testing.T) {
	(&sliptest.Function{
		Source:    `(boole 'quux 1 1)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
