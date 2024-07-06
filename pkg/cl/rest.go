// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Rest{Cdr: Cdr{Function: slip.Function{Name: "rest", Args: args}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "rest",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The value to take all but the first element of.",
				},
			},
			Return: "list|object",
			Text: `__rest__ returns the _rest_ if _arg_ is a _cons_, all but the first element if
_arg_ is a _list_, and _nil_ if _arg_ is _nil_ or an empty _list_.`,
			Examples: []string{
				"(rest nil) => nil",
				"(rest '(a . b) => b",
				"(rest '(a b c)) => (b c)",
			},
		}, &slip.CLPkg)
}

// Rest represents the rest function.
type Rest struct {
	Cdr
}
