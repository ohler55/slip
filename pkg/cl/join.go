// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Join{Function: slip.Function{Name: "join", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "join",
			Args: []*slip.DocArg{
				{
					Name: "separator",
					Type: "string",
					Text: "The separator for the join operation.",
				},
				{Name: "&rest"},
				{
					Name: "strings",
					Type: "string|list",
					Text: "The strings to join.",
				},
			},
			Return: "string",
			Text:   `__join__ a string from the _rest_ with each argument separated by the _separator_.`,
			Examples: []string{
				`(join " " "ab" '("cd" "ef")) => "ab cd ef"`,
			},
		}, &slip.CLPkg)
}

// Join represents the join function.
type Join struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Join) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, -1)
	var sep string
	if so, ok := args[0].(slip.String); ok {
		sep = string(so)
	} else {
		slip.PanicType("separator", args[0], "string")
	}
	return slip.String(strings.Join(f.joinArgs(nil, args[1:]), sep))
}

func (f *Join) joinArgs(sa []string, args slip.List) []string {
	for _, arg := range args {
		switch ta := arg.(type) {
		case slip.String:
			sa = append(sa, string(ta))
		case slip.Symbol:
			sa = append(sa, string(ta))
		case slip.List:
			sa = f.joinArgs(sa, ta)
		default:
			slip.PanicType("&rest", ta, "string", "symbol", "list")
		}
	}
	return sa
}
