// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Containsp{Function: slip.Function{Name: "containsp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "containsp",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "String to check if it contains _substr_.",
				},
				{
					Name: "substr",
					Type: "string",
					Text: "Sub string to check for.",
				},
			},
			Return: "boolean",
			Text:   `__containsp__ returns true if _string_ contains _substr_ and _nil_ otherwise.`,
			Examples: []string{
				`(containsp "abcdef" "cd") => t`,
			},
		}, &Pkg)
}

// Containsp represents the containsp function.
type Containsp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Containsp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 4)
	str := slip.MustBeString(args[0], "string")
	sub := slip.MustBeString(args[1], "substr")

	if v, has := slip.GetArgsKeyValue(args[2:], slip.Symbol(":ignore-case")); has && v != nil {
		str = strings.ToLower(str)
		sub = strings.ToLower(sub)
	}
	if strings.Contains(str, sub) {
		return slip.True
	}
	return nil
}
