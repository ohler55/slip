// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CharNe{Function: slip.Function{Name: "char/=", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "char/=",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "characters",
					Type: "character",
					Text: "The characters to compare.",
				},
			},
			Return: "nil",
			Text: `__char/=__ returns _true_ if _characters_ are equal.
If _characters_ is not a character an error is raised.`,
			Examples: []string{
				`(char/= #\A #\B #\C) => t`,
				`(char/= #\A #\B #\A) => nil`,
			},
		}, &slip.CLPkg)
}

// CharNe represents the char/= function.
type CharNe struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CharNe) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, -1)
	m := map[rune]bool{}
	var has bool
	for _, a := range args {
		if c, ok := a.(slip.Character); !ok {
			slip.PanicType("characters", a, "character")
		} else if _, has = m[rune(c)]; !has {
			m[rune(c)] = true
		} else {
			return nil
		}
	}
	return slip.True
}
