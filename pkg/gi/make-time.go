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

	slip.DefConstant(slip.Symbol("january"), slip.Fixnum(1), "The month of January as a fixnum.")
	slip.DefConstant(slip.Symbol("february"), slip.Fixnum(2), "The month of February as a fixnum.")
	slip.DefConstant(slip.Symbol("march"), slip.Fixnum(3), "The month of March as a fixnum.")
	slip.DefConstant(slip.Symbol("april"), slip.Fixnum(4), "The month of April as a fixnum.")
	slip.DefConstant(slip.Symbol("may"), slip.Fixnum(5), "The month of May as a fixnum.")
	slip.DefConstant(slip.Symbol("june"), slip.Fixnum(6), "The month of June as a fixnum.")
	slip.DefConstant(slip.Symbol("july"), slip.Fixnum(7), "The month of July as a fixnum.")
	slip.DefConstant(slip.Symbol("august"), slip.Fixnum(8), "The month of August as a fixnum.")
	slip.DefConstant(slip.Symbol("september"), slip.Fixnum(9), "The month of September as a fixnum.")
	slip.DefConstant(slip.Symbol("october"), slip.Fixnum(10), "The month of October as a fixnum.")
	slip.DefConstant(slip.Symbol("november"), slip.Fixnum(11), "The month of November as a fixnum.")
	slip.DefConstant(slip.Symbol("december"), slip.Fixnum(12), "The month of December as a fixnum.")
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
		slip.PanicType("year", args[pos], "fixnum")
	}
	pos++
	if month, ok = args[pos].(slip.Fixnum); !ok {
		slip.PanicType("month", args[pos], "fixnum")
	}
	pos++
	if day, ok = args[pos].(slip.Fixnum); !ok {
		slip.PanicType("day", args[pos], "fixnum")
	}
	pos++
	if pos < len(args) {
		if hour, ok = args[pos].(slip.Fixnum); !ok {
			slip.PanicType("hour", args[pos], "fixnum")
		}
		pos++
	}
	if pos < len(args) {
		if minute, ok = args[pos].(slip.Fixnum); !ok {
			slip.PanicType("minute", args[pos], "fixnum")
		}
		pos++
	}
	if pos < len(args) {
		if second, ok = args[pos].(slip.Fixnum); !ok {
			slip.PanicType("second", args[pos], "fixnum")
		}
		pos++
	}
	if pos < len(args) {
		if nsec, ok = args[pos].(slip.Fixnum); !ok {
			slip.PanicType("nanosecond", args[pos], "fixnum")
		}
		pos++
	}
	if pos < len(args) {
		loc = getLocArg(args[pos])
	}
	return slip.Time(time.Date(int(year), time.Month(month), int(day),
		int(hour), int(minute), int(second), int(nsec), loc))
}

func getLocArg(arg slip.Object) (loc *time.Location) {
	switch ta := arg.(type) {
	case slip.Symbol:
		switch strings.ToLower(string(ta)) {
		case "utc":
			loc = time.UTC
		case "local":
			loc = time.Local
		default:
			slip.PanicType("location", ta, "string", "symbol UTC", "symbol local")
		}
	case slip.String:
		var err error
		if loc, err = time.LoadLocation(string(ta)); err != nil {
			slip.NewPanic("failed to load time locations: %s", err)
		}
	default:
		slip.PanicType("location", ta, "string", "symbol")
	}
	return
}
