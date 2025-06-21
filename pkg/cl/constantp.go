// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Constantp{Function: slip.Function{Name: "constantp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "constantp",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "boolean",
			Text:   `__constantp__ returns _true_ if _object_ is a constant.`,
			Examples: []string{
				"(constantp 4) => t",
				"(constantp 'temp) => nil",
			},
		}, &slip.CLPkg)
}

// Constantp represents the constantp function.
type Constantp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Constantp) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	switch ta := args[0].(type) {
	case nil:
		return slip.True
	case slip.String, slip.Number:
		return slip.True
	case slip.Symbol:
		if 0 < len(ta) && ta[0] == ':' {
			return slip.True
		} else if _, _, ok := slip.GetConstant(string(ta)); ok {
			return slip.True
		}
	default:
		if ta == slip.True {
			return slip.True
		}
	}
	return
}
