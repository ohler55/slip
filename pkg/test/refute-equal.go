// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := RefuteEqual{Function: slip.Function{Name: "refute-equal", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "refute-equal",
			Args: []*slip.DocArg{
				{
					Name: "expect",
					Type: "object",
					Text: "The expected result to not be equal.",
				},
				{
					Name: "actual",
					Type: "object",
					Text: "The actual value.",
				},
				{Name: "&optional"},
				{
					Name: "message",
					Type: "string",
					Text: "An additional message to append to an error if generated.",
				},
			},
			Return: "nil",
			Text:   `__refute-equal__ panics if _expect_ is equal to _actual_.`,
			Examples: []string{
				`(refute-equal 3 (+ 1 1)) => nil`,
			},
		}, &slip.CLPkg)
}

// RefuteEqual represents the refute-equal function.
type RefuteEqual struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *RefuteEqual) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 3)
	if !slip.ObjectEqual(args[0], args[1]) {
		return nil
	}
	var (
		ansi = s.Get("*print-ansi*") != nil
		b    []byte
	)
	if ansi {
		b = append(b, ansiNormal...)
	}
	b = append(b, "expect: "...)
	b = slip.ObjectAppend(b, args[0])
	b = append(b, "\nactual: "...)
	if ansi {
		b = append(b, ansiRed...)
	}
	b = slip.ObjectAppend(b, args[1])
	b = append(b, '\n')
	if 2 < len(args) {
		if str, ok := args[2].(slip.String); ok {
			b = append(b, str...)
		} else {
			b = slip.ObjectAppend(b, args[2])
		}
		b = append(b, '\n')
	}
	panic(string(b))
}
