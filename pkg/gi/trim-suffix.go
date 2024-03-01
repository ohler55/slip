// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := TrimSuffix{Function: slip.Function{Name: "trim-suffix", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "trim-suffix",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "String to trim the suffix from.",
				},
				{
					Name: "suffix",
					Type: "string",
					Text: "Suffix to trim.",
				},
			},
			Return: "string",
			Text:   `__trim-suffix__ returns _string_ with _suffix_ removed.`,
			Examples: []string{
				`(trim-suffix "my-file.lisp" ".lisp") => "my-file"`,
			},
		}, &Pkg)
}

// TrimSuffix represents the trim-suffix function.
type TrimSuffix struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *TrimSuffix) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)
	str := slip.MustBeString(args[0], "string")
	suffix := slip.MustBeString(args[1], "suffix")

	return slip.String(strings.TrimSuffix(str, suffix))
}
