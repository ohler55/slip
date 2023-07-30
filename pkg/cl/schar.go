// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Schar{Char{Function: slip.Function{Name: "schar", Args: args}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "schar",
			Args: []*slip.DocArg{
				{Name: "string", Type: "string"},
				{Name: "index", Type: "fixnum"},
			},
			Return: "character",
			Text: `__schar__ returns the character at _index_ in _string_. Unlike Common LISP
this function does not support use with __setf__.`,
			Examples: []string{
				`(schar "abc" 1) => #\b`,
			},
		}, &slip.CLPkg)
}

// Schar represents the schar function.
type Schar struct {
	Char
}
