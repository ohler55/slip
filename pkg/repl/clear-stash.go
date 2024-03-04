// Copyright (c) 2024, Peter Ohler, All rights reserved.

package repl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ClearStash{Function: slip.Function{Name: "clear-stash", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "clear-stash",
			Args: []*slip.DocArg{
				{Name: "&key"},
				{
					Name: "start",
					Type: "fixnum",
					Text: "The start index of the stash to clear.",
				},
				{
					Name: "end",
					Type: "fixnum",
					Text: "The end index of the stash to clear.",
				},
			},
			Return: "nil",
			Text:   `__clear-stash__ from _start_ to _end_.`,
		}, &Pkg)
}

// ClearStash represents the clear-stash function.
type ClearStash struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *ClearStash) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return cleanStaskCall(f, &TheStash, s, args)
}

func cleanStaskCall(f slip.Object, stash *Stash, s *slip.Scope, args slip.List) slip.Object {
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
	stash.Clear(start, end)

	return nil
}
