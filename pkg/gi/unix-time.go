// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := UnixTime{Function: slip.Function{Name: "unix-time", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "unix-time",
			Args: []*slip.DocArg{
				{
					Name: "seconds",
					Type: "real",
					Text: "Unix seconds or units to create a time from.",
				},
				{Name: "&optional"},
				{
					Name: "units",
					Type: "keyword",
					Text: `The units the input seconds. Must
be :second, :millisecond, :microsecond, or :nanosecond.`,
				},
			},
			Return: "time",
			Text: `__unix-time__ returns the time from the Unix seconds or the specified
units since January 1, 1970 UTC.`,
			Examples: []string{
				`(unix-time 1657474161.0) => @2022-07-10T12:29:21-05:00)`,
			},
		}, &Pkg)
}

// UnixTime represents the unix-time function.
type UnixTime struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *UnixTime) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 2)
	r, ok := args[0].(slip.Real)
	if !ok {
		slip.TypePanic(s, depth, "seconds", args[0], "real")
	}
	if 1 < len(args) {
		switch args[1] {
		case slip.Symbol(":second"), slip.Symbol(":seconds"):
			result = slip.Time(time.Unix(0, int64(r.RealValue()*float64(time.Second))).UTC())
		case slip.Symbol(":millisecond"), slip.Symbol(":milliseconds"):
			result = slip.Time(time.Unix(0, int64(r.RealValue()*float64(time.Millisecond))).UTC())
		case slip.Symbol(":microsecond"), slip.Symbol(":microseconds"):
			result = slip.Time(time.Unix(0, int64(r.RealValue()*float64(time.Microsecond))).UTC())
		case slip.Symbol(":nanosecond"), slip.Symbol(":nanoseconds"):
			result = slip.Time(time.Unix(0, int64(r.RealValue())).UTC())
		default:
			slip.TypePanic(s, depth, "units", args[1], ":second", ":millisecond", ":micorsecond", ":nanosecond")
		}
	} else { // default to seconds
		result = slip.Time(time.Unix(0, int64(r.RealValue()*float64(time.Second))).UTC())
	}
	return
}
