// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Sleep{Function: slip.Function{Name: "sleep", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "sleep",
			Args: []*slip.DocArg{
				{
					Name: "seconds",
					Type: "real",
					Text: "The non-negative number of seconds to sleep.",
				},
			},
			Return: "nil",
			Text:   `__sleep__ the designated number of _seconds_.`,
			Examples: []string{
				"(sleep 1.2) => nil ;; sleeps for 1.2 seconds",
			},
		}, &slip.CLPkg)
}

// Sleep represents the sleep function.
type Sleep struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Sleep) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	real, ok := args[0].(slip.Real)
	if !ok {
		slip.TypePanic(s, depth, "seconds", args[0], "real")
	}
	time.Sleep(time.Duration(float64(time.Second) * real.RealValue()))

	return nil
}
