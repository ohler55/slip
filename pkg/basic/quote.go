// Copyright (c) 2022, Peter Ohler, All rights reserved.

package basic

import "github.com/ohler55/slip"

func init() {
	slip.Define(
		func(args slip.List) slip.Object { return &Quote{Function: slip.Function{Name: "quote", Args: args}} },
		&slip.FuncDoc{
			Name: "quote",
			Args: []*slip.DocArg{
				{
					Name:     "value",
					Type:     "object",
					Text:     "Any object.",
					Optional: false,
				},
			},
			Return: "object",
			Text:   `returns _value_ without evaluating it.`,
			Examples: []string{
				"(quote nil) => nil",
				"(quote (a . b)) => (a . b)",
				"'(a b) => (a b)",
			},
			HasKeys: false,
			HasRest: false,
		})
}

// Quote represents the quote function.
type Quote struct {
	slip.Function
}

// Eval the object.
func (f *Quote) Eval(s *slip.Scope, depth int) slip.Object {
	if len(f.Args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	s.Before(f, depth)
	defer s.After(f, depth)

	return f.Args[0]
}

// String representation of the Object.
func (f *Quote) String() string {
	return string(f.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (f *Quote) Append(b []byte) (out []byte) {
	if 0 < len(f.Args) {
		b = append(b, '\'')
		out = slip.Append(b, f.Args[0])
	}
	return
}
