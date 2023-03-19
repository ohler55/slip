// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi

import (
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := TimeUtc{Function: slip.Function{Name: "time-utc", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "time-utc",
			Args: []*slip.DocArg{
				{
					Name: "time",
					Type: "time",
					Text: "Time to change the location of.",
				},
			},
			Return: "time",
			Text:   `__time-utc__ returns a time with the location changed to UTC.`,
			Examples: []string{
				`(time-utc @2022-07-10T12:29:21-05:00) => @2022-07-10T17:29:21Z`,
			},
		}, &Pkg)
}

// TimeUtc represents the timeUtc function.
type TimeUtc struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *TimeUtc) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	t, ok := args[0].(slip.Time)
	if !ok {
		slip.PanicType("time", args[0], "time")
	}
	return slip.Time(time.Time(t).UTC())
}
