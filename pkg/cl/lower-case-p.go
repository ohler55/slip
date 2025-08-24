// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"unicode"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := LowerCaseP{Function: slip.Function{Name: "lower-case-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "lower-case-p",
			Args: []*slip.DocArg{
				{
					Name: "character",
					Type: "character",
					Text: "The character to check.",
				},
			},
			Return: "nil",
			Text: `__lower-case-p__ returns _true_ if _character_ is lower case
otherwise nil is returned. If _character_ is not a character an error is raised.`,
			Examples: []string{
				`(lower-case-p #\a) => t`,
				`(lower-case-p #\A) => nil`,
			},
		}, &slip.CLPkg)
}

// LowerCaseP represents the lower-case-p function.
type LowerCaseP struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *LowerCaseP) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	c, ok := args[0].(slip.Character)
	if !ok {
		slip.TypePanic(s, depth, "character", args[0], "character")
	}
	if unicode.IsLower(rune(c)) {
		return slip.True
	}
	return nil
}
