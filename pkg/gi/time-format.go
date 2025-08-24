// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi

import (
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := TimeFormat{Function: slip.Function{Name: "time-format", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "time-format",
			Args: []*slip.DocArg{
				{
					Name: "time",
					Type: "time",
					Text: "Time to change the location of.",
				},
				{
					Name: "layout",
					Type: "string",
					Text: "Layout of the formatted time.",
				},
			},
			Return: "string",
			Text:   `__time-format__ returns a string representation of the time.`,
			Examples: []string{
				`(time-format @2022-07-10T17:29:21Z "2006-01-02") => "2022-07-17"`,
			},
		}, &Pkg)
}

// TimeFormat represents the timeFormat function.
type TimeFormat struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *TimeFormat) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	t, ok := args[0].(slip.Time)
	if !ok {
		slip.TypePanic(s, depth, "time", args[0], "time")
	}
	var layout slip.String
	if layout, ok = args[1].(slip.String); !ok {
		slip.TypePanic(s, depth, "layout", args[1], "string")
	}
	return slip.String(time.Time(t).Format(string(layout)))
}
