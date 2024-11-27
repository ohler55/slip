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
					Text: "Time to return as Unix seconds.",
				},
				{Name: "&optional"},
				{
					Name: "units",
					Type: "keyword",
					Text: `The units the output should be in. Must
be :second, :millisecond, :microsecond, or :nanosecond.`,
				},
			},
			Return: "real",
			Text:   `__time-unix__ returns the number of seconds or the specified units since January 1, 1970 UTC.`,
			Examples: []string{
				`(time-unix @2022-07-10T12:29:21-05:00) => 1657474161.0`,
			},
		}, &Pkg)
}

// TimeUnix represents the time-unix function.
type TimeUnix struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *TimeUnix) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 2)
	t, ok := args[0].(slip.Time)
	if !ok {
		slip.PanicType("time", args[0], "time")
	}
	nano := time.Time(t).UnixNano()
	if 1 < len(args) {
		switch args[1] {
		case slip.Symbol(":second"), slip.Symbol(":seconds"):
			result = slip.DoubleFloat(float64(nano) / float64(time.Second))
		case slip.Symbol(":millisecond"), slip.Symbol(":milliseconds"):
			result = slip.DoubleFloat(float64(nano) / float64(time.Millisecond))
		case slip.Symbol(":microsecond"), slip.Symbol(":microseconds"):
			result = slip.DoubleFloat(float64(nano) / float64(time.Microsecond))
		case slip.Symbol(":nanosecond"), slip.Symbol(":nanoseconds"):
			result = slip.Fixnum(nano)
		default:
			slip.PanicType("units", args[1], ":second", ":millisecond", ":micorsecond", ":nanosecond")
		}
	} else { // default to seconds
		result = slip.DoubleFloat(float64(nano) / float64(time.Second))
	}
	return
}
