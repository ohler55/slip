// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"unicode"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Alphanumericp{Function: slip.Function{Name: "alphanumericp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "alphanumericp",
			Args: []*slip.DocArg{
				{
					Name: "character",
					Type: "character",
					Text: "The character to check.",
				},
			},
			Return: "boolean",
			Text: `__alphanumericp__ returns _true_ if _character_ is an alphabetic character
otherwise nil is returned. If _character_ is not a character an error is raised.`,
			Examples: []string{
				`(alphanumericp #\A) => t`,
				`(alphanumericp #\1) => t`,
				`(alphanumericp #\-) => t`,
			},
		}, &slip.CLPkg)
}

// Alphanumericp represents the alphanumericp function.
type Alphanumericp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Alphanumericp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	c, ok := args[0].(slip.Character)
	if !ok {
		slip.TypePanic(s, depth, "character", args[0], "character")
	}
	if unicode.IsLetter(rune(c)) || unicode.IsDigit(rune(c)) {
		return slip.True
	}
	return nil
}
