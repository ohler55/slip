// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Char{Function: slip.Function{Name: "char", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "char",
			Args: []*slip.DocArg{
				{Name: "string", Type: "string"},
				{Name: "index", Type: "fixnum"},
			},
			Return: "character",
			Text: `__char__ returns the character at _index_ in _string_. Unlike Common LISP
this function does not support use with __setf__.`,
			Examples: []string{
				`(char "abc" 1) => #\b`,
			},
		}, &slip.CLPkg)
}

// Char represents the char function.
type Char struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Char) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)
	str, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("string", args[0], "string")
	}
	var index int
	if si, ok2 := args[1].(slip.Integer); ok2 {
		index = int(si.Int64())
	} else {
		slip.PanicType("index", args[1], "integer")
	}
	ra := []rune(str)
	if index < 0 || len(ra) <= index {
		panic(fmt.Sprintf("index %d is outside the bound of 0 and %d", index, len(ra)))
	}
	return slip.Character(ra[index])
}
