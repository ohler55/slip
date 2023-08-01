// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"unicode"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CharNotGreaterp{Function: slip.Function{Name: "char-not-greaterp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "char-not-greaterp",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "characters",
					Type: "character",
					Text: "The characters to compare.",
				},
			},
			Return: "nil",
			Text: `__char-not-greaterp__ returns _true_ if all _characters_ are is ascending order ignoring case
and with equality allowed. If _characters_ is not a character an error is raised.`,
			Examples: []string{
				`(char-not-greaterp #\A #\b #\B #\C) => t`,
				`(char-not-greaterp #\b #\c) => nil`,
			},
		}, &slip.CLPkg)
}

// CharNotGreaterp represents the char-not-greaterp function.
type CharNotGreaterp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CharNotGreaterp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, -1)
	var prev rune
	for _, a := range args {
		if c, ok := a.(slip.Character); !ok {
			slip.PanicType("characters", a, "character")
		} else {
			lo := unicode.ToLower(rune(c))
			if lo < prev {
				return nil
			}
			prev = lo
		}
	}
	return slip.True
}
