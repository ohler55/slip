// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CharInt{Function: slip.Function{Name: "char-int", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "char-int",
			Args: []*slip.DocArg{
				{Name: "character", Type: "character"},
			},
			Return: "fixnum",
			Text:   `__char-int__ returns the character code as a _fixnum_ for _character_.`,
			Examples: []string{
				`(char-int #\A) => 65`,
			},
		}, &slip.CLPkg)
}

// CharInt represents the char-int function.
type CharInt struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CharInt) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	c, ok := args[0].(slip.Character)
	if !ok {
		slip.TypePanic(s, depth, "character", args[0], "character")
	}
	return slip.Fixnum(rune(c))
}
