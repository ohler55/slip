// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := TrimPrefix{Function: slip.Function{Name: "trim-prefix", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "trim-prefix",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "String to trim the prefix from.",
				},
				{
					Name: "prefix",
					Type: "string",
					Text: "Prefix to trim.",
				},
			},
			Return: "string",
			Text:   `__trim-prefix__ returns _string_ with _prefix_ removed.`,
			Examples: []string{
				`(trim-prefix "gi:fun" "gi:") => "fun"`,
			},
		}, &Pkg)
}

// TrimPrefix represents the trim-prefix function.
type TrimPrefix struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *TrimPrefix) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)
	str := slip.MustBeString(args[0], "string")
	prefix := slip.MustBeString(args[1], "prefix")

	return slip.String(strings.TrimPrefix(str, prefix))
}
