// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := EncodeUniversalTime{Function: slip.Function{Name: "encode-universal-time", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "encode-universal-time",
			Args: []*slip.DocArg{
				{Name: "second", Type: "fixnum"},
				{Name: "minute", Type: "fixnum"},
				{Name: "hour", Type: "fixnum"},
				{Name: "date", Type: "fixnum"},
				{Name: "month", Type: "fixnum"},
				{Name: "year", Type: "fixnum"},
				{Name: "&optional"},
				{
					Name: "time-zone",
					Type: "rational",
					Text: "The time zone to use for the universal time returned.",
				},
			},
			Return: "values",
			Text: `__encode-universal-time__ returns the number of seconds past 1900-01-01T00:00:00Z
for the time described by _second_, _minute_, _hour_, _date_, _month_, _year_ and optional the _time-zone_.
Note time zones in Common LISP are the reverse of current day time zones. West of UTC are positive and
not negative.`,
			Examples: []string{
				`(encode-universal-time 15, 14, 13, 12, 11, 2024) => xxxx`,
			},
		}, &slip.CLPkg)
}

// EncodeUniversalTime represents the encode-universal-time function.
type EncodeUniversalTime struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *EncodeUniversalTime) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 6, 7)

	loc := time.UTC
	if 6 < len(args) {
		if r, ok := args[6].(slip.Real); ok && -86400.0 <= r.RealValue() && r.RealValue() <= 86400.0 {
			loc = time.FixedZone("UTC", int(r.RealValue()*-3600.0))
		} else {
			slip.TypePanic(s, depth, "time-zone", args[6], "real")
		}
	}
	tm := time.Date(
		getFixnumArg(s, args[5], "year", depth),
		time.Month(getFixnumArg(s, args[4], "month", depth)),
		getFixnumArg(s, args[3], "date", depth),
		getFixnumArg(s, args[2], "hour", depth),
		getFixnumArg(s, args[1], "minute", depth),
		getFixnumArg(s, args[0], "second", depth),
		0,
		loc,
	)
	return slip.Fixnum(tm.Unix() + 2208988800)
}

func getFixnumArg(s *slip.Scope, arg slip.Object, use string, depth int) int {
	num, ok := arg.(slip.Fixnum)
	if !ok || num < 0 {
		slip.TypePanic(s, depth, use, arg, "non-negative fixnum")
	}
	return int(num)
}
