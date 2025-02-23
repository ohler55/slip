// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := DecodeUniversalTime{Function: slip.Function{Name: "decode-universal-time", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "decode-universal-time",
			Args: []*slip.DocArg{
				{
					Name: "time",
					Type: "time|fixnum",
					Text: "The time to return the elements of.",
				},
				{Name: "&optional"},
				{
					Name: "time-zone",
					Type: "rational",
					Text: "The time zone to use for the time elements returned.",
				},
			},
			Return: "values",
			Text: `__decode-universal-time__ returns the values
second, minute, hour, date, month, year, day, daylight-p, zone given a _time_ which can
be a __time__ or the number of seconds since 1900 as a _fixnum_. Note time zones in Common LISP
are the reverse of current day time zones. West of UTC are positive and not negative.`,
			Examples: []string{
				`(decode-universal-time @2024-11-12T13:14:15Z 5) => 15, 14, 13, 12, 11, 2024, 3, nil, 5`,
			},
		}, &slip.CLPkg)
}

// DecodeUniversalTime represents the decode-universal-time function.
type DecodeUniversalTime struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *DecodeUniversalTime) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 2)
	var tm time.Time
	switch ta := args[0].(type) {
	case slip.Time:
		tm = time.Time(ta)
	case slip.Fixnum:
		tm = time.Unix(int64(ta)-2208988800, 0)
	default:
		slip.PanicType("time", ta, "time", "fixnum")
	}
	if 1 < len(args) {
		if r, ok := args[1].(slip.Real); ok && -86400.0 <= r.RealValue() && r.RealValue() <= 86400.0 {
			tm = tm.In(time.FixedZone("UTC", int(r.RealValue()*-3600.0)))
		} else {
			slip.PanicType("time-zone", args[1], "real")
		}
	} else {
		tm = tm.In(time.Local)
	}
	_, zone := tm.Zone()
	var (
		dst slip.Object
		z   slip.Object
	)
	if len(args) == 1 && tm.IsDST() {
		dst = slip.True
	}
	if zone%3600 == 0 {
		z = slip.Fixnum(-zone / 3600)
	} else {
		z = slip.NewRatio(-int64(zone), 3600)
	}
	return slip.Values{
		slip.Fixnum(tm.Second()),
		slip.Fixnum(tm.Minute()),
		slip.Fixnum(tm.Hour()),
		slip.Fixnum(tm.Day()),
		slip.Fixnum(tm.Month()),
		slip.Fixnum(tm.Year()),
		slip.Fixnum((int(tm.Weekday()) + 6) % 7),
		dst,
		z,
	}
}
