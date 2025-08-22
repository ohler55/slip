// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"unicode"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CharLessp{Function: slip.Function{Name: "char-lessp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "char-lessp",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "characters",
					Type: "character",
					Text: "The characters to compare.",
				},
			},
			Return: "boolean",
			Text: `__char-lessp__ returns _true_ if all _characters_ are is ascending order ignoring case.
If _characters_ is not a character an error is raised.`,
			Examples: []string{
				`(char-lessp #\A #\b #\C) => t`,
				`(char-lessp #\B #\a) => nil`,
			},
		}, &slip.CLPkg)
}

// CharLessp represents the char-lessp function.
type CharLessp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CharLessp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, -1)
	var prev rune
	for _, a := range args {
		if c, ok := a.(slip.Character); !ok {
			slip.TypePanic(s, depth, "characters", a, "character")
		} else {
			lo := unicode.ToLower(rune(c))
			if lo <= prev {
				return nil
			}
			prev = lo
		}
	}
	return slip.True
}
