// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi

import (
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := TimeIn{Function: slip.Function{Name: "time-in", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "time-in",
			Args: []*slip.DocArg{
				{
					Name: "time",
					Type: "time",
					Text: "Time to change the location of.",
				},
				{
					Name: "location",
					Type: "string",
					Text: "Location for the time. (e.g. America/Toronto)",
				},
			},
			Return: "time",
			Text:   `__time-in__ returns a time with the location changed.`,
			Examples: []string{
				`(time-in @2022-07-10T17:29:21Z "EST") => @2022-07-10T12:29:21-05:00`,
			},
		}, &Pkg)
}

// TimeIn represents the timeIn function.
type TimeIn struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *TimeIn) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 2 {
		slip.PanicArgCount(f, 2, 2)
	}
	t, ok := args[0].(slip.Time)
	if !ok {
		slip.PanicType("time", args[0], "time")
	}
	loc := getLocArg(args[1])

	return slip.Time(time.Time(t).In(loc))
}
