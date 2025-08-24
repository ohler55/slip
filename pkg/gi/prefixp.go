// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Prefixp{Function: slip.Function{Name: "prefixp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "prefixp",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "String to check the prefix of.",
				},
				{
					Name: "prefix",
					Type: "string",
					Text: "Prefix to check.",
				},
			},
			Return: "boolean",
			Text:   `__prefixp__ returns true if _string_ has _prefix_.`,
			Examples: []string{
				`(prefixp "gi:fun" "gi:") => t`,
				`(prefixp "gi:fun" "cl:") => nil`,
			},
		}, &Pkg)
}

// Prefixp represents the prefixp function.
type Prefixp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Prefixp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	str := slip.MustBeString(args[0], "string")
	prefix := slip.MustBeString(args[1], "prefix")
	if strings.HasPrefix(str, prefix) {
		return slip.True
	}
	return nil
}
