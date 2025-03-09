// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := UniversalToTime{Function: slip.Function{Name: "universal-to-time", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "universal-to-time",
			Args: []*slip.DocArg{
				{
					Name: "universal-time",
					Type: "fixnum",
					Text: "universal-time to convert to time.",
				},
			},
			Return: "time",
			Text:   `__universal-to-time__ returns the number of seconds since 1900-01-01T00:00:00Z.`,
			Examples: []string{
				`(universal-to-time 3940406055) => @2024-11-12T13:14:15Z`,
			},
		}, &Pkg)
}

// UniversalToTime represents the universal-to-time function.
type UniversalToTime struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *UniversalToTime) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	ut, ok := args[0].(slip.Fixnum)
	if !ok {
		slip.PanicType("universal-time", args[0], "fixnum")
	}
	return slip.Time(time.Unix(int64(ut)-2208988800, 0).UTC())
}
