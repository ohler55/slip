// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"unicode"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := AlphaCharP{Function: slip.Function{Name: "alpha-char-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "alpha-char-p",
			Args: []*slip.DocArg{
				{
					Name: "character",
					Type: "character",
					Text: "The character to check.",
				},
			},
			Return: "nil",
			Text: `__alpha-char-p__ returns _true_ if _character_ is an alphabetic character
otherwise nil is returned. If _character_ is not a character an error is raised.`,
			Examples: []string{
				`(alpha-char-p #\A) => t`,
				`(alpha-char-p #\1) => nil`,
			},
		}, &slip.CLPkg)
}

// AlphaCharP represents the alpha-char-p function.
type AlphaCharP struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *AlphaCharP) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	c, ok := args[0].(slip.Character)
	if !ok {
		slip.TypePanic(s, depth, "character", args[0], "character")
	}
	if unicode.IsLetter(rune(c)) {
		return slip.True
	}
	return nil
}
