// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := TimeToUniversal{Function: slip.Function{Name: "time-to-universal", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "time-to-universal",
			Args: []*slip.DocArg{
				{
					Name: "time",
					Type: "time",
					Text: "time to convert to universal-time.",
				},
			},
			Return: "universal-time",
			Text:   `__time-to-universal__ returns the number of seconds since 1900-01-01T00:00:00Z.`,
			Examples: []string{
				`(time-to-universal @2024-11-12T13:14:15Z) => 3940406055`,
			},
		}, &Pkg)
}

// TimeToUniversal represents the time-to-universal function.
type TimeToUniversal struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *TimeToUniversal) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	st, ok := args[0].(slip.Time)
	if !ok {
		slip.TypePanic(s, depth, "time", args[0], "time")
	}
	return slip.Fixnum(time.Time(st).Unix() + 2208988800)
}
