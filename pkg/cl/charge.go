// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"unicode"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CharGe{Function: slip.Function{Name: "char>=", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "char>=",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "characters",
					Type: "character",
					Text: "The characters to compare.",
				},
			},
			Return: "boolean",
			Text: `__char>=__ returns _true_ if all _characters_ are is descending order with equality allowed.
If _characters_ is not a character an error is raised.`,
			Examples: []string{
				`(char>= #\C #\C #\A) => t`,
				`(char>= #\A #\B) => nil`,
			},
		}, &slip.CLPkg)
}

// CharGe represents the char>= function.
type CharGe struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CharGe) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, -1)
	prev := slip.Character(unicode.MaxRune + 1)
	for _, a := range args {
		if c, ok := a.(slip.Character); !ok {
			slip.PanicType("characters", a, "character")
		} else if c <= prev {
			prev = c
		} else {
			return nil
		}
	}
	return slip.True
}
