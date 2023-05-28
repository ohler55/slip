// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeStringOutputStream{Function: slip.Function{Name: "make-string-output-stream", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "make-string-output-stream",
			Args:   []*slip.DocArg{},
			Return: "string-stream",
			Text: `__make-string-output-stream__ returns an _output-stream_ backed by _string_. Unlike
common LISP the keyword _:element-type_ is not supported.`,
			Examples: []string{
				`(make-string-output-stream) => #<OUTPUT-STREAM>`,
			},
		}, &slip.CLPkg)
}

// MakeStringOutputStream represents the make-string-output-stream function.
type MakeStringOutputStream struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeStringOutputStream) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 0, 0)

	return &slip.OutputStream{Writer: &strings.Builder{}}
}
