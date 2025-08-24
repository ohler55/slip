// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := GetProperties{Function: slip.Function{Name: "get-properties", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "get-properties",
			Args: []*slip.DocArg{
				{
					Name: "plist",
					Type: "property list",
					Text: "A property list to lookup a value in.",
				},
				{
					Name: "indicator-list",
					Type: "list",
					Text: "The indicators for a property.",
				},
			},
			Return: "indicator, value, tail",
			Text: `__get-properties__ returns the indicator, value, and tail of the _plist_
starting with the matched perperty.`,
			Examples: []string{
				"(get-properties '(a 1 b 2 c 3) '(b c)) => b, 2, (b 2 c 3)",
			},
		}, &slip.CLPkg)
}

// GetProperties represents the get-properties function.
type GetProperties struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *GetProperties) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	var plist slip.List
	switch ta := args[0].(type) {
	case nil:
		// leave as an empty list
	case slip.List:
		plist = ta
	default:
		slip.TypePanic(s, depth, "plist", args[0], "property list")
	}
	indicators, ok := args[1].(slip.List)
	if !ok {
		slip.TypePanic(s, depth, "indicator-list", args[1], "list")
	}
	for i := 0; i < len(plist)-1; i += 2 {
		for _, ind := range indicators {
			if slip.ObjectEqual(ind, plist[i]) {
				return slip.Values{ind, plist[i+1], plist[i:]}
			}
		}
	}
	return slip.Values{nil, nil, nil}
}
