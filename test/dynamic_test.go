// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDynamicLambda(t *testing.T) {
	lambda := &slip.Dynamic{
		Function: slip.Function{
			Args: slip.List{slip.Fixnum(2), slip.Fixnum(1)},
		},
		Doc: &slip.FuncDoc{
			Args: []*slip.DocArg{{Name: "x"}, {Name: "y"}},
		},
		Forms: slip.List{slip.NewFunc("car", slip.List{slip.Symbol("x")})},
	}
	(&sliptest.Object{
		Target:    lambda,
		String:    "((lambda (x y) (car x)) 1 2)",
		Simple:    []interface{}{[]interface{}{"lambda", []interface{}{"x", "y"}, []interface{}{"car", "x"}}, 1, 2},
		Hierarchy: "function.t",
		Equals: []*sliptest.EqTest{
			{Other: lambda, Expect: false},
			{Other: slip.True, Expect: false},
		},
		// Eval: nil,
		Panics: true, // TBD remove
	}).Test(t)
}
