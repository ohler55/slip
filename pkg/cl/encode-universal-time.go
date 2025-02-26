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
	slip.ArgCountCheck(f, args, 6, 7)

	loc := time.UTC
	if 6 < len(args) {
		if r, ok := args[6].(slip.Real); ok && -86400.0 <= r.RealValue() && r.RealValue() <= 86400.0 {
			loc = time.FixedZone("UTC", int(r.RealValue()*-3600.0))
		} else {
			slip.PanicType("time-zone", args[6], "real")
		}
	}
	tm := time.Date(
		getFixnumArg(args[5], "year"),
		time.Month(getFixnumArg(args[4], "month")),
		getFixnumArg(args[3], "date"),
		getFixnumArg(args[2], "hour"),
		getFixnumArg(args[1], "minute"),
		getFixnumArg(args[0], "second"),
		0,
		loc,
	)
	return slip.Fixnum(tm.Unix() + 2208988800)
}

func getFixnumArg(arg slip.Object, use string) int {
	num, ok := arg.(slip.Fixnum)
	if !ok || num < 0 {
		slip.PanicType(use, arg, "non-negative fixnum")
	}
	return int(num)
}
