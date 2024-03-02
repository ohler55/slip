// Copyright (c) 2024, Peter Ohler, All rights reserved.

package repl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := NthHistory{Function: slip.Function{Name: "nth-history", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "nth-history",
			Args: []*slip.DocArg{
				{
					Name: "index",
					Type: "fixnum",
					Text: "Index of the history entry to return.",
				},
			},
			Return: "object",
			Text:   `__nth-history__ returns the form identified by _index_.`,
		}, &Pkg)
}

// NthHistory represents the nth-history function.
type NthHistory struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *NthHistory) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	var n int
	if num, ok := args[0].(slip.Fixnum); ok {
		n = int(num)
	} else {
		slip.PanicType("index", args[0], "fixnum")
	}
	form := TheHistory.Nth(TheHistory.Size() - n)
	code := slip.Read(form.Append(nil))
	switch len(code) {
	case 0:
		// leave as nil
	case 1:
		result = code[0]
	default:
		list := make(slip.List, len(code))
		for i, obj := range code {
			list[i] = obj
		}
		result = list
	}
	return
}
