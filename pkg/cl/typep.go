// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Typep{Function: slip.Function{Name: "typep", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "typep",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
				{
					Name: "type",
					Type: "symbol",
					Text: "The expected type of _object_.",
				},
			},
			Return: "nil",
			Text:   `__typep__ returns _true_ if _object_ is of type _type_.`,
			Examples: []string{
				"(typep 1 'fixnum) => t",
				"(typep 3.2 'fixnum) => nil",
			},
		}, &slip.CLPkg)
}

// Typep represents the typep function.
type Typep struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Typep) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	sym, ok := args[1].(slip.Symbol)
	if !ok {
		slip.TypePanic(s, depth, "type", args[1], "symbol")
	}
	switch ta := args[0].(type) {
	case nil:
		if strings.EqualFold("null", string(sym)) {
			return slip.True
		}
	case slip.List:
		if len(ta) == 0 && strings.EqualFold("null", string(sym)) {
			return slip.True
		}
		for _, h := range ta.Hierarchy() {
			if strings.EqualFold(string(h), string(sym)) {
				return slip.True
			}
		}
	default:
		for _, h := range ta.Hierarchy() {
			if strings.EqualFold(string(h), string(sym)) {
				return slip.True
			}
		}
	}
	return nil
}
