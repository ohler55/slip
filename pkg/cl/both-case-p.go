// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"unicode"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := BothCaseP{Function: slip.Function{Name: "both-case-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "both-case-p",
			Args: []*slip.DocArg{
				{
					Name: "character",
					Type: "character",
					Text: "The character to check.",
				},
			},
			Return: "nil",
			Text: `__both-case-p__ returns _true_ if _character_ has an counterpart case
otherwise nil is returned. If _character_ is not a character an error is raised.`,
			Examples: []string{
				`(both-case-p #\A) => t`,
				`(both-case-p #\1) => nil`,
			},
		}, &slip.CLPkg)
}

// BothCaseP represents the both-case-p function.
type BothCaseP struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *BothCaseP) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	c, ok := args[0].(slip.Character)
	if !ok {
		slip.TypePanic(s, depth, "character", args[0], "character")
	}
	if unicode.IsLower(rune(c)) || unicode.IsUpper(rune(c)) {
		return slip.True
	}
	return nil
}
