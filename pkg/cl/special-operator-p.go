// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SpecialOperatorP{Function: slip.Function{Name: "special-operator-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "special-operator-p",
			Args: []*slip.DocArg{
				{
					Name: "symbol",
					Type: "symbol",
					Text: "The symbol to check.",
				},
			},
			Return: "boolean",
			Text:   `__special-operator-p__ returns true if _symbol_ is a special operator.`,
			Examples: []string{
				"(special-operator-p 'let) => t",
				"(special-operator-p 'car) => nil",
			},
		}, &slip.CLPkg)
}

// SpecialOperatorP represents the special-operator-p function.
type SpecialOperatorP struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SpecialOperatorP) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	sym, ok := args[0].(slip.Symbol)
	if !ok {
		slip.PanicType("symbol", args[0], "symbol")
	}
	if strings.EqualFold("block", string(sym)) ||
		strings.EqualFold("catch", string(sym)) ||
		strings.EqualFold("function", string(sym)) ||
		strings.EqualFold("go", string(sym)) ||
		strings.EqualFold("if", string(sym)) ||
		strings.EqualFold("let", string(sym)) ||
		strings.EqualFold("let*", string(sym)) ||
		strings.EqualFold("multiple-value-call", string(sym)) ||
		strings.EqualFold("multiple-value-progn", string(sym)) ||
		strings.EqualFold("progn", string(sym)) ||
		strings.EqualFold("progv", string(sym)) ||
		strings.EqualFold("quote", string(sym)) ||
		strings.EqualFold("return-from", string(sym)) ||
		strings.EqualFold("setq", string(sym)) ||
		strings.EqualFold("tagbody", string(sym)) ||
		strings.EqualFold("the", string(sym)) ||
		strings.EqualFold("throw", string(sym)) ||
		strings.EqualFold("unwind-protect", string(sym)) {

		return slip.True
	}
	return nil
}
