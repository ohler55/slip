// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Suffixp{Function: slip.Function{Name: "suffixp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "suffixp",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "String to check the suffix of.",
				},
				{
					Name: "suffix",
					Type: "string",
					Text: "Suffix to check.",
				},
			},
			Return: "boolean",
			Text:   `__suffixp__ returns true if _string_ has _suffix_.`,
			Examples: []string{
				`(suffixp "my-file.lisp" ".lisp") => t`,
				`(suffixp "my-file.lisp" ".txt") => nil`,
			},
		}, &Pkg)
}

// Suffixp represents the suffixp function.
type Suffixp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Suffixp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)
	str := slip.MustBeString(args[0], "string")
	suffix := slip.MustBeString(args[1], "suffix")
	if strings.HasSuffix(str, suffix) {
		return slip.True
	}
	return nil
}
