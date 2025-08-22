// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi

import (
	"strings"
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeTime{Function: slip.Function{Name: "make-time", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-time",
			Args: []*slip.DocArg{
				{
					Name: "year",
					Type: "fixnum",
					Text: "Year of the time to make.",
				},
				{
					Name: "month",
					Type: "fixnum",
					Text: "Month of the time to make.",
				},
				{
					Name: "day",
					Type: "fixnum",
					Text: "Day of the month of the time to make.",
				},
				{Name: slip.AmpOptional},
				{
					Name: "hour",
					Type: "fixnum",
					Text: "Hour of the time to make.",
				},
				{
					Name: "minute",
					Type: "fixnum",
					Text: "Minute of the time to make.",
				},
				{
					Name: "second",
					Type: "fixnum",
					Text: "Second of the time to make.",
				},
				{
					Name: "nanosecond",
					Type: "fixnum",
					Text: "Nanosecond of the time to make.",
				},
				{
					Name: "location",
					Type: "string",
					Text: "Location for the time. (e.g. America/Toronto)",
				},
			},
			Return: "time",
			Text:   `__make-time__ make a new time with the provided elements.`,
			Examples: []string{
				`(make-time 2022 july 17 20 53 24 123456789 "EST") => @2022-07-17T20:53:24.123456789-05:00`,
			},
		}, &Pkg)
}

// MakeTime represents the makeTime function.
type MakeTime struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeTime) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 3, 8)
	var (
		year   slip.Fixnum
		month  slip.Fixnum
		day    slip.Fixnum
		hour   slip.Fixnum
		minute slip.Fixnum
		second slip.Fixnum
		nsec   slip.Fixnum
		loc    = time.UTC
		ok     bool
	)
	pos := 0
	if year, ok = args[pos].(slip.Fixnum); !ok {
		slip.TypePanic(s, depth, "year", args[pos], "fixnum")
	}
	pos++
	if month, ok = args[pos].(slip.Fixnum); !ok {
		slip.TypePanic(s, depth, "month", args[pos], "fixnum")
	}
	pos++
	if day, ok = args[pos].(slip.Fixnum); !ok {
		slip.TypePanic(s, depth, "day", args[pos], "fixnum")
	}
	pos++
	if pos < len(args) {
		if hour, ok = args[pos].(slip.Fixnum); !ok {
			slip.TypePanic(s, depth, "hour", args[pos], "fixnum")
		}
		pos++
	}
	if pos < len(args) {
		if minute, ok = args[pos].(slip.Fixnum); !ok {
			slip.TypePanic(s, depth, "minute", args[pos], "fixnum")
		}
		pos++
	}
	if pos < len(args) {
		if second, ok = args[pos].(slip.Fixnum); !ok {
			slip.TypePanic(s, depth, "second", args[pos], "fixnum")
		}
		pos++
	}
	if pos < len(args) {
		if nsec, ok = args[pos].(slip.Fixnum); !ok {
			slip.TypePanic(s, depth, "nanosecond", args[pos], "fixnum")
		}
		pos++
	}
	if pos < len(args) {
		loc = getLocArg(s, args[pos], depth)
	}
	return slip.Time(time.Date(int(year), time.Month(month), int(day),
		int(hour), int(minute), int(second), int(nsec), loc))
}

func getLocArg(s *slip.Scope, arg slip.Object, depth int) (loc *time.Location) {
	switch ta := arg.(type) {
	case slip.Symbol:
		switch strings.ToLower(string(ta)) {
		case "utc":
			loc = time.UTC
		case "local":
			loc = time.Local
		default:
			slip.TypePanic(s, depth, "location", ta, "string", "symbol UTC", "symbol local")
		}
	case slip.String:
		var err error
		if loc, err = time.LoadLocation(string(ta)); err != nil {
			slip.NewPanic("failed to load time locations: %s", err)
		}
	default:
		slip.TypePanic(s, depth, "location", ta, "string", "symbol")
	}
	return
}
