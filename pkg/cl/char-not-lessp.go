// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"unicode"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CharNotLessp{Function: slip.Function{Name: "char-not-lessp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "char-not-lessp",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "characters",
					Type: "character",
					Text: "The characters to compare.",
				},
			},
			Return: "nil",
			Text: `__char-not-lessp__ returns _true_ if all _characters_ are is descending order ignoring case
and with equality allowed. If _characters_ is not a character an error is raised.`,
			Examples: []string{
				`(char-not-lessp #\C #\b #\B #\A) => t`,
				`(char-not-lessp #\a #\b) => nil`,
			},
		}, &slip.CLPkg)
}

// CharNotLessp represents the char-not-lessp function.
type CharNotLessp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CharNotLessp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, -1)
	prev := unicode.MaxRune + 1
	for _, a := range args {
		if c, ok := a.(slip.Character); !ok {
			slip.PanicType("characters", a, "character")
		} else {
			lo := unicode.ToLower(rune(c))
			if prev < lo {
				return nil
			}
			prev = lo
		}
	}
	return slip.True
}
