// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Function{Function: slip.Function{Name: "name", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "function",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "symbol|lambda",
					Text: "Any function symbol or lambda expression.",
				},
			},
			Return: "function",
			Text:   `__function__ returns the function bound to _name_ or the evaluated lambda expression.`,
			Examples: []string{
				"(function car) => #<FUNCTION CAR>",
				"#'car => #<FUNCTION CAR>",
				"#'(lambda (x) (car x)) => #<FUNCTION (LAMBDA (X))>",
			},
		}, &slip.CLPkg)
}

// Function represents the function function.
type Function struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Function) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	switch ta := args[0].(type) {
	case slip.Symbol:
		return slip.FindFunc(string(ta))
	case slip.List:
		if 1 < len(ta) {
			if sym, ok := ta[0].(slip.Symbol); ok {
				if strings.EqualFold("lambda", string(sym)) {
					lambdaDef := slip.ListToFunc(s, ta, depth+1)
					return s.Eval(lambdaDef, depth).(*slip.Lambda)
				}
			}
		}
	}
	panic(fmt.Sprintf("|%s| is not a function name or lambda", slip.ObjectString(args[0])))
}

// String representation of the Object.
func (f *Function) String() string {
	return string(f.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (f *Function) Append(b []byte) (out []byte) {
	if 0 < len(f.Args) {
		b = append(b, "#'"...)
		out = slip.Append(b, f.Args[0])
	}
	return
}
