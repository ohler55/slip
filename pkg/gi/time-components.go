// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := TimeComponents{Function: slip.Function{Name: "time-components", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "time-components",
			Args: []*slip.DocArg{
				{
					Name: "time",
					Type: "time",
					Text: "The time to a duration to.",
				},
			},
			Return: "list",
			Text: `__time-components__ returns a list of the time components as
(year month day hour minute second nanosecond weekday).`,
			Examples: []string{
				`(time-components @2024-11-12T13:14:15.123456789Z) =>`,
				`  (2024 11 12 13 14 15 123456789 "Tuesday")`,
			},
		}, &Pkg)
}

// TimeComponents represents the time-components function.
type TimeComponents struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *TimeComponents) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	t, ok := args[0].(slip.Time)
	if !ok {
		slip.TypePanic(s, depth, "time", args[0], "time")
	}
	return slip.TimeComponents(s, t, nil, depth)
}
