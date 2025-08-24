// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := WriteToString{Function: slip.Function{Name: "write-to-string", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "write-to-string",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The _object_ to be written to a string.",
				},
				{Name: "&key"},
				{Name: "array", Type: "boolean"},
				{Name: "base", Type: "fixnum"},
				{Name: "case", Type: ":upcase :downcase :capitalize"},
				{Name: "circle", Type: "boolean"},
				{Name: "escape", Type: "boolean"},
				{Name: "gensym", Type: "boolean"},
				{Name: "length", Type: "fixnum or nil"},
				{Name: "level", Type: "fixnum or nil"},
				{Name: "lines", Type: "fixnum or nil"},
				{Name: "miser-width", Type: "fixnum or nil"},
				{Name: "pretty", Type: "boolean"},
				{Name: "radix", Type: "boolean"},
				{Name: "readably", Type: "boolean"},
				{Name: "right-margin", Type: "fixnum or nil"},
			},
			Return: "string",
			Text: `__write-to-string__ a string representation of the _object_ is returned.
Output is produced according to the default print variables unless overridden by the
keyword parameters the same as the __write__ function.
`,
			Examples: []string{
				`(write-to-string 123) => "123"`,
			},
		}, &slip.CLPkg)
}

// WriteToString represents the write-to-string function.
type WriteToString struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *WriteToString) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	b, _, _ := writeBuf(f, s, args, false, depth)

	return slip.String(b)
}
