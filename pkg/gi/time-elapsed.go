// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := TimeElapsed{Function: slip.Function{Name: "time-elapsed", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "time-elapsed",
			Args: []*slip.DocArg{
				{
					Name: "start",
					Type: "time",
					Text: "The start time.",
				},
				{
					Name: "end",
					Type: "time",
					Text: "The end time.",
				},
			},
			Return: "double-float",
			Text:   `__time-elapsed__ returns the elapsed time in seconds from the _start_ to the _end_ time.`,
			Examples: []string{
				`(time-elapsed @2024-11-24T12:00:00Z @2024-11-24T13:14:15Z) => 4455`,
			},
		}, &Pkg)
}

// TimeElapsed represents the time-elapsed function.
type TimeElapsed struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *TimeElapsed) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	start, ok := args[0].(slip.Time)
	if !ok {
		slip.TypePanic(s, depth, "start", args[0], "time")
	}
	var end slip.Time
	if end, ok = args[1].(slip.Time); !ok {
		slip.TypePanic(s, depth, "end", args[1], "time")
	}
	return slip.DoubleFloat(float64(time.Time(end).Sub(time.Time(start))) / float64(time.Second))
}
