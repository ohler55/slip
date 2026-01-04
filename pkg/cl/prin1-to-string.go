// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Prin1ToString{Function: slip.Function{Name: "prin1-to-string", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "prin1-to-string",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The _object_ to be printed into a string.",
				},
			},
			Return: "string",
			Text: `__prin1-to-string__ writes _object_ to a string.
The string is produced as if _*print-escape*_ is _true_ and _*print-readably*_ is _true_.`,
			Examples: []string{
				`(prin1-to-string 123) => "123"`,
				`(prin1-to-string #\A) => "#\A"`,
			},
		}, &slip.CLPkg)
}

// Prin1ToString represents the prin1-to-string function.
type Prin1ToString struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Prin1ToString) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)

	p := *slip.DefaultPrinter()
	p.ScopedUpdate(s)
	p.Escape = true
	p.Readably = true

	obj := args[0]
	var b []byte
	if sa, ok := obj.(slip.ScopedAppender); ok {
		b = sa.ScopedAppend(b, s, &p, 0)
	} else {
		b = p.Append(b, obj, 0)
	}
	return slip.String(b)
}
