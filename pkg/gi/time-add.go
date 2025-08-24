// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := TimeAdd{Function: slip.Function{Name: "time-add", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "time-add",
			Args: []*slip.DocArg{
				{
					Name: "time",
					Type: "time",
					Text: "The time to a duration to.",
				},
				{
					Name: "duration",
					Type: "real",
					Text: "The seconds to add to the time argument.",
				},
			},
			Return: "time",
			Text:   `__time-add__ returns the a new time that is _duration_ seconds from _time_.`,
			Examples: []string{
				`(time-add @2024-11-24T12:00:00Z 4455) => @2024-11-24T13:14:15Z`,
			},
		}, &Pkg)
}

// TimeAdd represents the time-add function.
type TimeAdd struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *TimeAdd) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	t, ok := args[0].(slip.Time)
	if !ok {
		slip.TypePanic(s, depth, "time", args[0], "time")
	}
	var dur slip.Real
	if dur, ok = args[1].(slip.Real); !ok {
		slip.TypePanic(s, depth, "duration", args[1], "real")
	}
	return slip.Time(time.Time(t).Add(time.Duration(dur.RealValue() * float64(time.Second))))
}
