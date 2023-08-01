// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"unicode"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CharEqual{Function: slip.Function{Name: "char-equal", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "char-equal",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "characters",
					Type: "character",
					Text: "The characters to compare.",
				},
			},
			Return: "nil",
			Text: `__char-equal__ returns _true_ if _characters_ are equal ignoring case.
If _characters_ is not a character an error is raised.`,
			Examples: []string{
				`(char-equal #\A #\a) => t`,
				`(char-equal #\A #\a #\B) => nil`,
			},
		}, &slip.CLPkg)
}

// CharEqual represents the char-equal function.
type CharEqual struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CharEqual) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, -1)
	var target rune
	for i, a := range args {
		if c, ok := a.(slip.Character); !ok {
			slip.PanicType("characters", a, "character")
		} else if i == 0 {
			target = unicode.ToLower(rune(c))
		} else if target != unicode.ToLower(rune(c)) {
			return nil
		}
	}
	return slip.True
}
