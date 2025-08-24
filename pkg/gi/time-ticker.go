// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := TimeTicker{Function: slip.Function{Name: "time-ticker", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "time-ticker",
			Args: []*slip.DocArg{
				{
					Name: "duration",
					Type: "real",
					Text: "time in seconds to wait between ticks.",
				},
			},
			Return: "channel",
			Text: `__time-ticker__ returns a channel the current time will be pushed to
on _duration_ seconds intervals.`,
			Examples: []string{
				`(time-ticker 1.0) => #<time-channel>`,
			},
		}, &Pkg)
}

// TimeTicker represents the time-ticker function.
type TimeTicker struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *TimeTicker) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	dur, ok := args[0].(slip.Real)
	if !ok {
		slip.TypePanic(s, depth, "duration", args[0], "real")
	}
	return TimeChannel(time.Tick(time.Duration(float64(time.Second) * dur.RealValue())))
}
