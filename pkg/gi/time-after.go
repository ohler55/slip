// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := TimeAfter{Function: slip.Function{Name: "time-after", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "time-after",
			Args: []*slip.DocArg{
				{
					Name: "duration",
					Type: "real",
					Text: "time in seconds to wait before pushing the current time.",
				},
			},
			Return: "channel",
			Text: `__time-after__ returns a channel the current time will be pushed to
after waiting _duration_ seconds.`,
			Examples: []string{
				`(time-after 1.0) => #<time-channel>`,
			},
		}, &Pkg)
}

// TimeAfter represents the time-after function.
type TimeAfter struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *TimeAfter) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	dur, ok := args[0].(slip.Real)
	if !ok {
		slip.TypePanic(s, depth, "duration", args[0], "real")
	}
	return TimeChannel(time.After(time.Duration(float64(time.Second) * dur.RealValue())))
}
