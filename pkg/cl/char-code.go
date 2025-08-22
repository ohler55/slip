// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CharCode{Function: slip.Function{Name: "char-code", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "char-code",
			Args: []*slip.DocArg{
				{Name: "character", Type: "character"},
			},
			Return: "fixnum",
			Text:   `__char-code__ returns the character code for _character_.`,
			Examples: []string{
				`(char-code #\A) => 65`,
			},
		}, &slip.CLPkg)
}

// CharCode represents the char-code function.
type CharCode struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CharCode) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	c, ok := args[0].(slip.Character)
	if !ok {
		slip.TypePanic(s, depth, "character", args[0], "character")
	}
	return slip.Fixnum(rune(c))
}
