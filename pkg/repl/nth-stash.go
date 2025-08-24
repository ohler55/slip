// Copyright (c) 2024, Peter Ohler, All rights reserved.

package repl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := NthStash{Function: slip.Function{Name: "nth-stash", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "nth-stash",
			Args: []*slip.DocArg{
				{
					Name: "index",
					Type: "fixnum",
					Text: "Index of the stash entry to return.",
				},
			},
			Return: "object",
			Text:   `__nth-stash__ returns the form identified by _index_.`,
		}, &Pkg)
}

// NthStash represents the nth-stash function.
type NthStash struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *NthStash) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return nthStashCall(f, &TheStash, s, args)
}

func nthStashCall(f slip.Object, stash *Stash, s *slip.Scope, args slip.List) (result slip.Object) {
	slip.CheckArgCount(s, 0, f, args, 1, 1)
	var n int
	if num, ok := args[0].(slip.Fixnum); ok {
		n = int(num)
	} else {
		slip.TypePanic(s, 0, "index", args[0], "fixnum")
	}
	form := stash.Nth(stash.Size() - n)
	code := slip.Read(form.Append(nil), s)
	switch len(code) {
	case 0:
		// leave as nil
	case 1:
		result = code[0]
	default:
		list := make(slip.List, len(code))
		copy(list, code)
		result = list
	}
	return
}
