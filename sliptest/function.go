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

	// Panics if true indicated the call to .Eval() should panic.
	Panics bool

	// Result of the function call.
	Result slip.Object
}

// Test the object test specification.
func (tf *Function) Test(t *testing.T) {
	obj := slip.CompileString(tf.Source)
	scope := tf.Scope
	if scope == nil {
		scope = slip.NewScope()
	}
	if tf.Panics {
		tt.Panic(t, func() { obj.Eval(scope, 0) }, tf.Source)
	} else {
		tf.Result = obj.Eval(scope, 0)
		tt.Equal(t, tf.Expect, slip.ObjectString(tf.Result), tf.Source)
	}
}
