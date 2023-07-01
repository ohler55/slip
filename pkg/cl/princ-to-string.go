// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := PrincToString{Function: slip.Function{Name: "princ-to-string", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "princ-to-string",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The _object_ to be printed into a string.",
				},
			},
			Return: "string",
			Text: `__princ-to-string__ writes _object_ to a string.
The string is produced as if _*print-escape*_ is _false_ and _*print-readably*_ is _false_.`,
			Examples: []string{
				`(princ-to-string 123) => "123"`,
				`(princ-to-string #\A) => "A"`,
			},
		}, &slip.CLPkg)
}

// PrincToString represents the princ-to-string function.
type PrincToString struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *PrincToString) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)

	p := *slip.DefaultPrinter()
	p.Escape = false
	p.Readably = false

	return slip.String(p.Append([]byte{}, args[0], 0))
}
