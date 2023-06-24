// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := AssertNil{Function: slip.Function{Name: "assert-nil", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "assert-nil",
			Args: []*slip.DocArg{
				{
					Name: "value",
					Type: "object",
					Text: "The value to verify as _nil_.",
				},
				{Name: "&optional"},
				{
					Name: "message",
					Type: "string",
					Text: "An additional message to append to an error if generated.",
				},
			},
			Return: "nil",
			Text:   `__assert-nil__ panics if _value_ is nat _nil_.`,
			Examples: []string{
				`(assert-nil (car '())) => nil`,
			},
		}, &slip.CLPkg)
}

// AssertNil represents the assert-nil function.
type AssertNil struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *AssertNil) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 2)
	if args[0] == nil {
		return nil
	}
	var (
		ansi = s.Get("*print-ansi*") != nil
		b    []byte
	)
	if ansi {
		b = append(b, ansiNormal...)
	}
	b = append(b, "expect: nil\nactual: "...)
	if ansi {
		b = append(b, ansiRed...)
	}
	b = slip.ObjectAppend(b, args[0])
	b = append(b, '\n')
	if 1 < len(args) {
		if str, ok := args[1].(slip.String); ok {
			b = append(b, str...)
		} else {
			b = slip.ObjectAppend(b, args[1])
		}
		b = append(b, '\n')
	}
	panic(string(b))
}
