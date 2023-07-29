// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"unicode"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CharUpcase{Function: slip.Function{Name: "char-upcase", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "char-upcase",
			Args: []*slip.DocArg{
				{
					Name: "character",
					Type: "character",
					Text: "The character to convert.",
				},
			},
			Return: "nil",
			Text: `__char-upcase__ returns _character_ converted to upper case.
If _character_ is not a character an error is raised.`,
			Examples: []string{
				`(char-upcase #\a) => #\A`,
				`(char-upcase #\A) => #\A`,
			},
		}, &slip.CLPkg)
}

// CharUpcase represents the char-upcase function.
type CharUpcase struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CharUpcase) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	c, ok := args[0].(slip.Character)
	if !ok {
		slip.PanicType("character", args[0], "character")
	}
	return slip.Character(unicode.ToUpper(rune(c)))
}
