// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"unicode"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CharGt{Function: slip.Function{Name: "char>", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "char>",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "characters",
					Type: "character",
					Text: "The characters to compare.",
				},
			},
			Return: "boolean",
			Text: `__char>__ returns _true_ if all _characters_ are is descending order.
If _characters_ is not a character an error is raised.`,
			Examples: []string{
				`(char> #\C #\B #\A) => t`,
				`(char> #\A #\A) => nil`,
			},
		}, &slip.CLPkg)
}

// CharGt represents the char> function.
type CharGt struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CharGt) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, -1)
	prev := slip.Character(unicode.MaxRune + 1)
	for _, a := range args {
		if c, ok := a.(slip.Character); !ok {
			slip.PanicType("characters", a, "character")
		} else if c < prev {
			prev = c
		} else {
			return nil
		}
	}
	return slip.True
}
