// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"unicode"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CharNotEqual{Function: slip.Function{Name: "char-not-equal", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "char-not-equal",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "characters",
					Type: "character",
					Text: "The characters to compare.",
				},
			},
			Return: "nil",
			Text: `__char-not-equal__ returns _true_ if _characters_ are equal.
If _characters_ is not a character an error is raised.`,
			Examples: []string{
				`(char-not-equal #\A #\B #\C) => t`,
				`(char-not-equal #\A #\B #\A) => nil`,
			},
		}, &slip.CLPkg)
}

// CharNotEqual represents the char-not-equal function.
type CharNotEqual struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CharNotEqual) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, -1)
	m := map[rune]bool{}
	var has bool
	for _, a := range args {
		if c, ok := a.(slip.Character); !ok {
			slip.PanicType("characters", a, "character")
		} else {
			lo := unicode.ToLower(rune(c))
			if _, has = m[lo]; has {
				return nil
			}
			m[lo] = true
		}
	}
	return slip.True
}
