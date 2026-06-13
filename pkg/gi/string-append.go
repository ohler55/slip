// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func defStringAppend() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StringAppend{Function: slip.Function{Name: "string-append", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "string-append",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "strings",
					Type: "string",
					Text: "String designators to append.",
				},
			},
			Return: "string",
			Text:   `__string-append__ append all _strings_ and return the result.`,
			Examples: []string{
				`(string-append "Abc" 'def) => "Abcdef"`,
			},
		}, &Pkg)
}

// StringAppend represents the string-append function.
type StringAppend struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *StringAppend) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	var buf []byte
	for _, a := range args {
		buf = append(buf, slip.MustBeString(a, "strings")...)
	}
	return slip.String(buf)
}
