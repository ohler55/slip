// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := TimeUnix{Function: slip.Function{Name: "time-unix", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "time-unix",
			Args: []*slip.DocArg{
				{
					Name: "time",
					Type: "time",
					Text: "Time to return as Unix nanoseconds.",
				},
			},
			Return: "time",
			Text:   `__time-unix__ returns the number of nanoseconds since January 1, 1970 UTC.`,
			Examples: []string{
				`(time-unix @2022-07-10T12:29:21-05:00) => 1657474161000000000`,
			},
		}, &Pkg)
}

// TimeUnix represents the time-unix function.
type TimeUnix struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *TimeUnix) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	t, ok := args[0].(slip.Time)
	if !ok {
		slip.PanicType("time", args[0], "time")
	}
	return slip.Fixnum(time.Time(t).UnixNano())
}
