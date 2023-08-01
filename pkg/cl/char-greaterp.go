// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"unicode"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CharGreaterp{Function: slip.Function{Name: "char-greaterp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "char-greaterp",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "characters",
					Type: "character",
					Text: "The characters to compare.",
				},
			},
			Return: "nil",
			Text: `__char-greaterp__ returns _true_ if all _characters_ are is descending order ignoring case.
If _characters_ is not a character an error is raised.`,
			Examples: []string{
				`(char-greaterp #\C #\b #\A) => t`,
				`(char-greaterp #\a #\b) => nil`,
			},
		}, &slip.CLPkg)
}

// CharGreaterp represents the char-greaterp function.
type CharGreaterp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CharGreaterp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, -1)
	prev := unicode.MaxRune + 1
	for _, a := range args {
		if c, ok := a.(slip.Character); !ok {
			slip.PanicType("characters", a, "character")
		} else {
			lo := unicode.ToLower(rune(c))
			if prev <= lo {
				return nil
			}
			prev = lo
		}
	}
	return slip.True
}
