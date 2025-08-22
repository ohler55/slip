// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := TrimSpace{Function: slip.Function{Name: "trim-space", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "trim-space",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "String trim space characters (space, tab, new-line, etc).",
				},
			},
			Return: "string",
			Text:   `__trim-space__ returns a string with space characters on the left and right removed.`,
			Examples: []string{
				`(trim-space " abc \n") => "abc"`,
			},
		}, &Pkg)
}

// TrimSpace represents the trim-space function.
type TrimSpace struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *TrimSpace) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	str := slip.MustBeString(args[0], "string")

	return slip.String(strings.TrimSpace(str))
}
