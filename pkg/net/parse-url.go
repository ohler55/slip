// Copyright (c) 2026, Peter Ohler, All rights reserved.

package net

import (
	"net/url"

	"github.com/ohler55/slip"
)

func defParseURL() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ParseURL{Function: slip.Function{Name: "parse-url", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "parse-url",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "Parse a string into a url instance.",
				},
			},
			Return: "url",
			Text:   `__parse-url__ parses a _string_ and creates a _url-flavor_ instance.`,
			Examples: []string{
				`(parse-url "http://localhost:12345") => #<url-flavor 12345>`,
			},
		}, &Pkg)
}

// ParseURL represents the parse-url function.
type ParseURL struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *ParseURL) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	u, err := url.Parse(slip.MustBeString(args[0], "string"))
	if err != nil {
		slip.ErrorPanic(s, depth, "URL parse error. %s", err)
	}
	return MakeURL(u)
}
