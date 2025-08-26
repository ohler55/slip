// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ContinueWhopper{Function: slip.Function{Name: "continue-whopper", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "continue-whopper",
			Args: []*slip.DocArg{
				{Name: slip.AmpRest},
				{
					Name: "args",
					Type: "object",
					Text: `The remaining arguments to the method.`,
				},
			},
			Return: "object",
			Text:   `__continue-whopper__ continues with the rest of the daemon methods using the arguments provided.`,
			Examples: []string{
				"(defwhopper (blueberry :rot) () (continue-whopper))",
			},
		}, &Pkg)
}

// ContinueWhopper represents the continueWhopper function.
type ContinueWhopper struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *ContinueWhopper) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	loc, _ := s.Get("~whopper-location~").(*slip.WhopLoc)
	if loc == nil {
		slip.ErrorPanic(s, depth, "%s called outside an around method daemon.", f.Name)
	}
	return loc.Continue(s, args, depth)
}
