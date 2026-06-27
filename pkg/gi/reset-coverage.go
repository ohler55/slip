// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func defResetCoverage() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ResetCoverage{Function: slip.Function{Name: "reset-coverage", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "reset-coverage",
			Args: []*slip.DocArg{
				{Name: "&optional"},
				{
					Name: "hard",
					Type: "boolean",
					Text: "If true all functions previously identified for coverage are forgotten.",
				},
			},
			Return: "nil",
			Text: `__reset-coverage__ resets all coverage counts to zero. If _hard_ is non-nil
then all identified coverage function are forgotten.`,
			Examples: []string{
				`(reset-coverage) => nil`,
			},
		}, &Pkg)
}

// ResetCoverage represents the reset-coverage function.
type ResetCoverage struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ResetCoverage) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 0, 1)

	slip.ResetCoverage(0 < len(args) && args[0] != nil)

	return nil
}
