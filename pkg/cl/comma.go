// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Comma{Function: slip.Function{Name: "comma", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "comma",
			Args: []*slip.DocArg{
				{
					Name: "value",
					Type: "object",
					Text: "Any object.",
				},
			},
			Return: "object",
			Text: `__comma__ is only allowed within a backquote. It evaluates and returns the value
of it's argument despite being in a backquoted expression.`,
			Examples: []string{
				"(setq x 3)",
				"`(comma x) => 3",
				"`(1 ,x) => (1 2)",
			},
		}, &slip.CLPkg)
}

// Comma represents the comma function.
type Comma struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Comma) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	return args[0]
}

// String representation of the Object.
func (f *Comma) String() string {
	return string(f.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (f *Comma) Append(b []byte) (out []byte) {
	if 0 < len(f.Args) {
		b = append(b, ',')
		out = slip.Append(b, f.Args[0])
	}
	return
}

// SpecialPrefix returns the prefix character for writing.
func (f *Comma) SpecialPrefix() string {
	return ","
}
