// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"regexp"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := AssertMatch{Function: slip.Function{Name: "assert-match", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "assert-match",
			Args: []*slip.DocArg{
				{
					Name: "regexp",
					Type: "string",
					Text: "A regexp string to match against the value.",
				},
				{
					Name: "value",
					Type: "object",
					Text: "The value to match against.",
				},
				{Name: "&optional"},
				{
					Name: "message",
					Type: "string",
					Text: "An additional message to append to an error if generated.",
				},
			},
			Return: "nil",
			Text:   `__assert-match__ panics if _regexp_ does nat match to _value_.`,
			Examples: []string{
				`(assert-match "^[0-9a-f]+$" "abc123") => nil`,
			},
		}, &Pkg)
}

// AssertMatch represents the assert-match function.
type AssertMatch struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *AssertMatch) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 3)
	var (
		rx    *regexp.Regexp
		value string
	)
	if ss, ok := args[0].(slip.String); ok {
		rx = regexp.MustCompile(string(ss))
	} else {
		slip.TypePanic(s, depth, "regexp", args[0], "string")
	}
	switch ta := args[1].(type) {
	case slip.String:
		value = string(ta)
	case slip.Symbol:
		value = string(ta)
	default:
		value = slip.ObjectString(args[1])
	}
	if rx.MatchString(value) {
		return nil
	}
	var (
		ansi = s.Get("*print-ansi*") != nil
		b    []byte
	)
	if ansi {
		b = append(b, ansiNormal...)
	}
	b = append(b, "expect: /"...)
	b = append(b, rx.String()...)
	b = append(b, "/\nactual: "...)
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
