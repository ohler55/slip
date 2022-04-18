// Copyright (c) 2022, Peter Ohler, All rights reserved.

package sliptest

import (
	"strings"
	"testing"

	"github.com/ohler55/slip"
	// Pull in all functions.
	_ "github.com/ohler55/slip/pkg"

	"github.com/ohler55/ojg/alt"
	"github.com/ohler55/ojg/pretty"
	"github.com/stretchr/testify/require"
)

// EqTest is used for specifying what the result of comparing Other to a
// target object.
type EqTest struct {
	Other  slip.Object
	Expect bool
}

// Object test structure is used for testing slip.Objects.
type Object struct {

	// Scope is the Scope to use instead of creating a new Scope if it is
	// non-nil.
	Scope *slip.Scope

	// Target of the test.
	Target slip.Object

	// String is the expected output from calling .String() and .Append().
	String string

	// Simple is the expected result from calling .Simplify(). If of type
	// error a panic is expected. If the values is a function then that
	// function is called with the result of Simplify() and should fail the
	// test if the simplified value is not as expected.
	Simple interface{}

	// Hierarchy is the expected return from .Hierarchy() but joined with a
	// '.' character.
	Hierarchy string

	// Equals are the other values to try with the .Equal() function.
	Equals []*EqTest

	// Selfies are the functions that should return the object's type. They
	// represent the super-types checks which are implementation checks on
	// interface().
	Selfies []func() slip.Symbol

	// Eval is the expected result from a call to .Eval().
	Eval slip.Object

	// Panics if true indicated the call to .Eval() should panic.
	Panics bool
}

// Test the object test specification.
func (to *Object) Test(t *testing.T) {
	if 0 < len(to.String) && to.String[0] == '/' && to.String[len(to.String)-1] == '/' {
		require.Regexp(t, to.String[1:len(to.String)-1], to.Target.String(), "String() output")
		require.Regexp(t, to.String[1:len(to.String)-1], string(to.Target.Append([]byte{})), "Append() output")
	} else {
		require.Equal(t, to.String, to.Target.String(), "String() output")
		require.Equal(t, to.String, string(to.Target.Append([]byte{})), "Append() output")
	}
	switch ts := to.Simple.(type) {
	case error:
		require.Panics(t, func() { _ = to.Target.Simplify() })
	case func(*testing.T, interface{}):
		ts(t, to.Target.Simplify())
	default:
		simp := to.Target.Simplify()
		diff := alt.Compare(to.Simple, simp)
		require.Nil(t, diff, "Simplify difference at %v for %s vs %s", diff, pretty.SEN(to.Simple), pretty.SEN(simp))
	}
	if 0 < len(to.Hierarchy) {
		var hb []byte
		for i, sym := range to.Target.Hierarchy() {
			if 0 < i {
				hb = append(hb, '.')
			}
			hb = append(hb, strings.ToLower(string(sym))...)
		}
		require.Equal(t, to.Hierarchy, string(hb))
	}
	for _, et := range to.Equals {
		if et.Expect {
			require.True(t, to.Target.Equal(et.Other),
				"Equal (%T)%s vs (%T)%s", to.Target, to.Target, et.Other, et.Other)
		} else {
			require.False(t, to.Target.Equal(et.Other),
				"Not Equal (%T)%s vs (%T)%s", to.Target, to.Target, et.Other, et.Other)
		}
	}
	self := to.Target.Hierarchy()[0]
	for _, f := range to.Selfies {
		require.Equal(t, self, f())
	}
	if to.Scope == nil {
		to.Scope = slip.NewScope()
	}
	if to.Panics {
		require.Panics(t, func() { to.Target.Eval(to.Scope, 0) })
	} else {
		result := to.Target.Eval(to.Scope, 0)
		require.True(t, slip.ObjectEqual(to.Eval, result),
			"Eval returned '%s' but expected '%s'", slip.ObjectString(result), slip.ObjectString(to.Eval))
	}
}
