// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CharEq{Function: slip.Function{Name: "char=", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "char=",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "characters",
					Type: "character",
					Text: "The characters to compare.",
				},
			},
			Return: "nil",
			Text: `__char=__ returns _true_ if _characters_ are equal.
If _characters_ is not a character an error is raised.`,
			Examples: []string{
				`(char= #\A #\A) => t`,
				`(char= #\A #\a) => nil`,
			},
		}, &slip.CLPkg)
}

// CharEq represents the char= function.
type CharEq struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CharEq) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, -1)
	var target slip.Character
	for i, a := range args {
		if c, ok := a.(slip.Character); !ok {
			slip.PanicType("characters", a, "character")
		} else if i == 0 {
			target = c
		} else if target != c {
			return nil
		}
	}
	return slip.True
}
