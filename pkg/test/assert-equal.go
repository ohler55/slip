// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"unicode/utf8"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := AssertEqual{Function: slip.Function{Name: "assert-equal", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "assert-equal",
			Args: []*slip.DocArg{
				{
					Name: "expect",
					Type: "object",
					Text: "The expected result.",
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
			Text:   `__assert-equal__ panics if _expect_ is not equal to _actual_.`,
			Examples: []string{
				`(assert-equal 3 (+ 1 2)) => nil`,
			},
		}, &Pkg)
}

// AssertEqual represents the assert-equal function.
type AssertEqual struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *AssertEqual) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 3)
	if slip.ObjectEqual(args[0], args[1]) {
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
	if expect, ok := args[0].(slip.String); ok && ansi {
		var actual slip.String
		if actual, ok = args[1].(slip.String); ok {
			b = append(b, '"')
			b = append(b, expect...)
			b = append(b, "\"\nactual: \""...)
			re := []rune(expect)
			ra := []rune(actual)
			for i, r := range re {
				if r != ra[i] {
					b = append(b, ansiRed...)
				}
				b = utf8.AppendRune(b, ra[i])
			}
			b = append(b, '"')
		}
	} else {
		b = slip.ObjectAppend(b, args[0])
		b = append(b, "\nactual: "...)
		if ansi {
			b = append(b, ansiRed...)
		}
		b = slip.ObjectAppend(b, args[1])
	}
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
