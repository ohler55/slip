// Copyright (c) 2022, Peter Ohler, All rights reserved.

package sliptest

import (
	"testing"

	"github.com/ohler55/slip"
	// Pull in all functions.
	_ "github.com/ohler55/slip/pkg"

	"github.com/ohler55/ojg/tt"
)

// Function test structure is used for testing slip functions.
type Function struct {

	// Scope is the Scope to use instead of creating a new Scope if it is
	// non-nil.
	Scope *slip.Scope

	// Source of the test as LISP code.
	Source string

	// Expect is the expected result from a call to .Eval().
	Expect string

	// Validate is an optional function to use for validating the result.
	Validate func(t *testing.T, v slip.Object)

	// Readaby if true print readably.
	Readably bool

	// Panics if true indicated the call to .Eval() should panic.
	Panics bool

	// PanicType is the expected panic type as a Symbol. If nil then no check.
	PanicType slip.Object

	// Result of the function call.
	Result slip.Object
}

// Test the object test specification.
func (tf *Function) Test(t *testing.T) {
	scope := tf.Scope
	if scope == nil {
		scope = slip.NewScope()
	}
	switch {
	case tf.PanicType != nil:
		r := tt.Panic(t, func() {
			obj := slip.CompileString(tf.Source)
			obj.Eval(scope, 0)
		}, tf.Source)
		so, ok := r.(slip.Object)
		tt.Equal(t, true, ok, "expected a panic of %s not a %T", tf.PanicType, r)
		tt.Equal(t, tf.PanicType, so.Hierarchy()[0], "expected a panic of %s not a %s", tf.PanicType, so.Hierarchy()[0])
	case tf.Panics:
		tt.Panic(t, func() {
			obj := slip.CompileString(tf.Source)
			obj.Eval(scope, 0)
		}, tf.Source)
	default:
		obj := slip.CompileString(tf.Source)
		tf.Result = scope.Eval(obj, 0)
		switch {
		case tf.Validate != nil:
			tf.Validate(t, tf.Result)
		case tf.Readably:
			p := *slip.DefaultPrinter()
			p.Readably = true
			p.RightMargin = 80
			p.Pretty = true
			tt.Equal(t, tf.Expect, string(p.Append(nil, tf.Result, 0)), tf.Source)
		default:
			p := *slip.DefaultPrinter()
			p.RightMargin = 80
			p.Pretty = true
			tt.Equal(t, tf.Expect, string(p.Append(nil, tf.Result, 0)), tf.Source)
		}
	}
}
