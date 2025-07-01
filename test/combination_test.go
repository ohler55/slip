// Copyright (c) 2025, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestCombination(t *testing.T) {
	// The type of caller doesn't matter for this test.
	fi := slip.MustFindFunc("1+", &slip.CLPkg)
	caller := fi.Create(slip.List{slip.Fixnum(3)}).(slip.Caller)

	combo := slip.Combination{
		From: slip.FindClass("fixnum"),
	}
	tt.Equal(t, true, combo.Empty())
	combo.Primary = caller
	tt.Equal(t, false, combo.Empty())
	combo.Before = caller
	combo.After = caller
	combo.Wrap = caller
	tt.Equal(t, `{after: true before: true from: fixnum primary: true whopper: true}`, pretty.SEN(combo.Simplify()))
}
