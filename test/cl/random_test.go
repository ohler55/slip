// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"math/big"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestRandomFixnum(t *testing.T) {
	scope := slip.NewScope()
	result := slip.CompileString("(random 10)", scope).Eval(scope, 0)
	tt.SameType(t, slip.Fixnum(0), result)
	num := int(result.(slip.Fixnum))
	tt.Equal(t, true, 0 <= num)
	tt.Equal(t, true, num < 10)

	result = slip.CompileString("(random 10 *random-state*)", scope).Eval(scope, 0)
	tt.SameType(t, slip.Fixnum(0), result)
	num = int(result.(slip.Fixnum))
	tt.Equal(t, true, 0 <= num)
	tt.Equal(t, true, num < 10)

	result = slip.CompileString("(random 10 nil)", scope).Eval(scope, 0)
	tt.SameType(t, slip.Fixnum(0), result)
	num = int(result.(slip.Fixnum))
	tt.Equal(t, true, 0 <= num)
	tt.Equal(t, true, num < 10)

	result = slip.CompileString("(random 10 t)", scope).Eval(scope, 0)
	tt.SameType(t, slip.Fixnum(0), result)
	num = int(result.(slip.Fixnum))
	tt.Equal(t, true, 0 <= num)
	tt.Equal(t, true, num < 10)
}

func TestRandomSingleFloat(t *testing.T) {
	scope := slip.NewScope()
	result := slip.CompileString("(random 10s+0)", scope).Eval(scope, 0)
	tt.SameType(t, slip.SingleFloat(0.0), result)
	num := float64(result.(slip.SingleFloat))
	tt.Equal(t, true, 0.0 <= num)
	tt.Equal(t, true, num < 10.0)

	result = slip.CompileString("(random 10s+0 *random-state*)", scope).Eval(scope, 0)
	tt.SameType(t, slip.SingleFloat(0.0), result)
	num = float64(result.(slip.SingleFloat))
	tt.Equal(t, true, 0.0 <= num)
	tt.Equal(t, true, num < 10.0)
}

func TestRandomDoubleFloat(t *testing.T) {
	scope := slip.NewScope()
	result := slip.CompileString("(random 10d+0)", scope).Eval(scope, 0)
	tt.SameType(t, slip.DoubleFloat(0.0), result)
	num := float64(result.(slip.DoubleFloat))
	tt.Equal(t, true, 0.0 <= num)
	tt.Equal(t, true, num < 10.0)

	result = slip.CompileString("(random 10d+0 *random-state*)", scope).Eval(scope, 0)
	tt.SameType(t, slip.DoubleFloat(0.0), result)
	num = float64(result.(slip.DoubleFloat))
	tt.Equal(t, true, 0.0 <= num)
	tt.Equal(t, true, num < 10.0)
}

func TestRandomLongFloat(t *testing.T) {
	scope := slip.NewScope()
	result := slip.CompileString("(random 10L+0)", scope).Eval(scope, 0)
	tt.SameType(t, (*slip.LongFloat)(big.NewFloat(0.0)), result)
	num := (*big.Float)(result.(*slip.LongFloat))
	tt.Equal(t, -1, big.NewFloat(0.0).Cmp(num))
	tt.Equal(t, true, 0 <= big.NewFloat(10.0).Cmp(num))
}

func TestRandomBignum(t *testing.T) {
	scope := slip.NewScope()
	result := slip.CompileString("(random (- 10000000000000000010 10000000000000000000))", scope).Eval(scope, 0)
	tt.SameType(t, (*slip.Bignum)(big.NewInt(0)), result)
	num := (*big.Int)(result.(*slip.Bignum))
	tt.Equal(t, true, big.NewInt(0).Cmp(num) <= 0)
	tt.Equal(t, true, 0 < big.NewInt(10).Cmp(num))
}

func TestRandomArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(random)`,
		Panics: true,
	}).Test(t)
}

func TestRandomNotReal(t *testing.T) {
	(&sliptest.Function{
		Source: `(random t)`,
		Panics: true,
	}).Test(t)
}
