// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi

import (
	"time"

	"github.com/ohler55/slip"
)

func defParseTime() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ParseTime{Function: slip.Function{Name: "parse-time", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "parse-time",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "A string to parse.",
				},
				{Name: "&optional"},
				{
					Name: "layout",
					Type: "string",
					Text: `The layout or format of the string to parse.
Default is RFC3339Nano or 2006-01-02T15:04:05.999999999Z07:00 (see go time documentation
for additional details.`,
				},
				{
					Name: "location",
					Type: "string",
					Text: "The location to parse the time in, e.g., MST or UTC. Default is UTC.",
				},
			},
			Return: "time",
			Text: `__parse-time__ returns the a new time parsed from _string_ following the _layout_
and in _location_.`,
			Examples: []string{
				`(parse-time "2024-11-24T12:00:00Z") => @2024-11-24T12:00:00Z`,
			},
		}, &Pkg)
}

// ParseTime represents the parse-time function.
type ParseTime struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ParseTime) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 3)
	str, ok := args[0].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "string", args[0], "string")
	}
	layout := time.RFC3339Nano
	location := time.UTC
	if 1 < len(args) {
		var ss slip.String
		if ss, ok = args[1].(slip.String); ok {
			layout = string(ss)
		} else {
			slip.TypePanic(s, depth, "layout", args[1], "string")
		}
		if 2 < len(args) {
			if ss, ok = args[2].(slip.String); ok {
				var err error
				if location, err = time.LoadLocation(string(ss)); err != nil {
					slip.ErrorPanic(s, depth, "%s", err)
				}
			} else {
				slip.TypePanic(s, depth, "location", args[2], "string")
			}
		}
	}
	t, err := time.ParseInLocation(layout, string(str), location)
	if err != nil {
		slip.ErrorPanic(s, depth, "%s", err)
	}
	return slip.Time(t)
}
