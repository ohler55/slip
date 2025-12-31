// Copyright (c) 2025, Peter Ohler, All rights reserved.

package sliptest

import (
	"testing"

	"github.com/ohler55/slip"
	// Pull in all functions.
	_ "github.com/ohler55/slip/pkg"

	"github.com/ohler55/ojg/tt"
)

// Macro test structure is used for testing slip macros. Unlike Function which
// uses CompileString, Macro uses ReadString followed by Eval, which is
// necessary for testing macros that need to be expanded at read/eval time.
type Macro struct {

	// Scope is the Scope to use instead of creating a new Scope if it is
	// non-nil.
	Scope *slip.Scope

	// Source of the test as LISP code.
	Source string

	// Expect is the expected result from a call to .Eval().
	Expect string

	// Validate is an optional function to use for validating the result.
	Validate func(t *testing.T, v slip.Object)

	// Readably if true print readably.
	Readably bool

	// Array if true print arrays.
	Array bool

	// Panics if true indicated the call to .Eval() should panic.
	Panics bool

	// PanicType is the expected panic type as a Symbol. If nil then no check.
	PanicType slip.Object

	// Result of the macro evaluation.
	Result slip.Object
}

// Test the macro test specification.
func (tm *Macro) Test(t *testing.T) {
	scope := tm.Scope
	if scope == nil {
		scope = slip.NewScope()
	}
	switch {
	case tm.PanicType != nil:
		r := tt.Panic(t, func() {
			code := slip.ReadString(tm.Source, scope)
			code.Eval(scope, nil)
		}, tm.Source)
		so, ok := r.(slip.Object)
		tt.Equal(t, true, ok, "expected a panic of %s not a %T", tm.PanicType, r)
		if tm.PanicType != so.Hierarchy()[0] {
			if p, ok := r.(*slip.Panic); ok && p.Condition != nil {
				so = p.Condition
			}
		}
		tt.Equal(t, tm.PanicType, so.Hierarchy()[0], "expected a panic of %s not a %s (%s)",
			tm.PanicType, so.Hierarchy()[0], so)
		if tm.Validate != nil {
			tm.Validate(t, so)
		}
	case tm.Panics:
		tt.Panic(t, func() {
			code := slip.ReadString(tm.Source, scope)
			code.Eval(scope, nil)
		}, tm.Source)
	default:
		code := slip.ReadString(tm.Source, scope)
		tm.Result = code.Eval(scope, nil)
		if tm.Validate != nil {
			tm.Validate(t, tm.Result)
		} else {
			p := *slip.DefaultPrinter()
			p.Readably = tm.Readably
			p.Array = tm.Array
			p.RightMargin = 80
			p.Pretty = true
			tt.Equal(t, tm.Expect, string(p.Append(nil, tm.Result, 0)), tm.Source)
		}
	}
}
