// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Character{Function: slip.Function{Name: "character", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "character",
			Args: []*slip.DocArg{
				{Name: "name", Type: "string|symbol|character"},
			},
			Return: "fixnum",
			Text:   `__character__ returns the character designated by _character_.`,
			Examples: []string{
				`(character #\a) => #\a`,
				`(character "a") => #\a`,
				`(character 'a) => #\a`,
			},
		}, &slip.CLPkg)
}

// Character represents the character function.
type Character struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Character) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	switch ta := args[0].(type) {
	case slip.Symbol:
		ra := []rune(ta)
		if len(ra) == 1 {
			return slip.Character(ra[0])
		}
	case slip.String:
		ra := []rune(ta)
		if len(ra) == 1 {
			return slip.Character(ra[0])
		}
	case slip.Character:
		return ta
	default:
		slip.TypePanic(s, depth, "character", args[0], "string", "symbol", "character")
	}
	panic(slip.NewParseError("%s is not a valid character designator", args[0]))
}
