// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := String{Function: slip.Function{Name: "string", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "string",
			Args: []*slip.DocArg{
				{
					Name: "value",
					Type: "object",
					Text: "The value to convert to a string.",
				},
			},
			Return: "string",
			Text:   `__string__ returns _value_ coerced or converted to a string.`,
			Examples: []string{
				`(string 'abc) => "abc"`,
				`(string 123) => "123"`,
			},
		}, &slip.CLPkg)
}

// String represents the string function.
type String struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *String) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	switch ta := args[0].(type) {
	case slip.String:
		result = ta
	case slip.Symbol:
		result = slip.String(ta)
	case slip.Character:
		result = slip.String([]rune{rune(ta)})
	default:
		result = slip.String(slip.Append(nil, ta))
	}
	return
}
