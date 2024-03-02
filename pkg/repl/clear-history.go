// Copyright (c) 2024, Peter Ohler, All rights reserved.

package repl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ClearHistory{Function: slip.Function{Name: "clear-history", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "clear-history",
			Args: []*slip.DocArg{
				{Name: "&key"},
				{
					Name: "start",
					Type: "fixnum",
					Text: "The start index of the history to clear.",
				},
				{
					Name: "end",
					Type: "fixnum",
					Text: "The end index of the history to clear.",
				},
			},
			Return: "nil",
			Text:   `__clear-history__ from _start_ to _end_.`,
		}, &Pkg)
}

// ClearHistory represents the clear-history function.
type ClearHistory struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *ClearHistory) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 0, 4)
	start := 0
	end := -1
	if v, has := slip.GetArgsKeyValue(args, slip.Symbol(":start")); has {
		if num, ok := v.(slip.Fixnum); ok {
			start = int(num)
		} else {
			slip.PanicType(":start", v, "fixnum")
		}
	}
	if v, has := slip.GetArgsKeyValue(args, slip.Symbol(":end")); has {
		if num, ok := v.(slip.Fixnum); ok {
			end = int(num)
		} else {
			slip.PanicType(":end", v, "fixnum")
		}
	}
	TheHistory.Clear(start, end)

	return nil
}
