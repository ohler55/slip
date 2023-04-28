// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCondBasic(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("value", slip.Fixnum(3))
	(&sliptest.Function{
		Scope: scope,
		Source: `(cond
                   ((not (integerp value)) 'unknown)
                   ((zerop value) 'zero)
                   ((evenp value) 'even)
                   (t 'odd))`,
		Expect: "odd",
	}).Test(t)
	scope.Let("value", slip.Fixnum(2))
	(&sliptest.Function{
		Scope: scope,
		Source: `(cond
                   ((not (integerp value)) 'unknown)
                   ((zerop value) 'zero)
                   ((evenp value) 'even)
                   (t 'odd))`,
		Expect: "even",
	}).Test(t)
	scope.Let("value", slip.DoubleFloat(2.5))
	(&sliptest.Function{
		Scope: scope,
		Source: `(cond
                   ((not (integerp value)) 'unknown)
                   ((zerop value) 'zero)
                   ((evenp value) 'even)
                   (t 'odd))`,
		Expect: "unknown",
	}).Test(t)
}

func TestCondNoMatch(t *testing.T) {
	(&sliptest.Function{
		Source: `(cond (nil 1))`,
		Expect: "nil",
	}).Test(t)
}

func TestCondEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(cond)`,
		Expect: "nil",
	}).Test(t)
}

func TestCondBadClause(t *testing.T) {
	(&sliptest.Function{
		Source: `(cond t)`,
		Panics: true,
	}).Test(t)
}
