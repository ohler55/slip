// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMakeRandomStateCurrent(t *testing.T) {
	// A copy of the *random-state* should produce the same random number.
	scope := slip.NewScope()
	_ = slip.CompileString("(setq my-state (make-random-state))", scope).Eval(scope, 0)
	current := slip.CompileString("(random 100)", scope).Eval(scope, 0)
	result := slip.CompileString("(random 100 my-state)", scope).Eval(scope, 0)
	tt.Equal(t, current, result)
}

func TestMakeRandomStateTrue(t *testing.T) {
	scope := slip.NewScope()
	current := slip.CompileString("*random-state*", scope).Eval(scope, 0)
	result := slip.CompileString("(make-random-state t)", scope).Eval(scope, 0)

	// TBD there is some finite possibility that these will match but that is
	// very unlikely.
	tt.NotEqual(t, current.String(), result.String())
}

func TestMakeRandomStateCopy(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.CompileString("(setq state-1 (make-random-state))", scope).Eval(scope, 0)
	_ = slip.CompileString("(setq state-2 (make-random-state state-1))", scope).Eval(scope, 0)
	r1 := slip.CompileString("(random 100 state-1)", scope).Eval(scope, 0)
	r2 := slip.CompileString("(random 100 state-2)", scope).Eval(scope, 0)
	tt.Equal(t, r1, r2)
}

func TestMakeRandomStateArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-random-state t t)`,
		Panics: true,
	}).Test(t)
}

func TestMakeRandomStateNotState(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-random-state 7)`,
		Panics: true,
	}).Test(t)
}
