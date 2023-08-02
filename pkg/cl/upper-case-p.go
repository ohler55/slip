// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"unicode"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := UpperCaseP{Function: slip.Function{Name: "upper-case-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "upper-case-p",
			Args: []*slip.DocArg{
				{
					Name: "character",
					Type: "character",
					Text: "The character to check.",
				},
			},
			Return: "nil",
			Text: `__upper-case-p__ returns _true_ if _character_ is upper case
otherwise nil is returned. If _character_ is not a character an error is raised.`,
			Examples: []string{
				`(upper-case-p #\a) => t`,
				`(upper-case-p #\A) => nil`,
			},
		}, &slip.CLPkg)
}

// UpperCaseP represents the upper-case-p function.
type UpperCaseP struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *UpperCaseP) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	c, ok := args[0].(slip.Character)
	if !ok {
		slip.PanicType("character", args[0], "character")
	}
	if unicode.IsUpper(rune(c)) {
		return slip.True
	}
	return nil
}
