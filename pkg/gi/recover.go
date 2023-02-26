// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Recover{Function: slip.Function{Name: "recover", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "recover",
			Args: []*slip.DocArg{
				{
					Name: "symbol",
					Type: "symbol",
					Text: "The symbol to assign the recovered value to on recovery.",
				},
				{
					Name: "on-recover",
					Type: "object",
					Text: "The form to evaluate on recovery.",
				},
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "object",
					Text: "The forms to evaluate.",
				},
			},
			Return: "object",
			Text: `__recover__ catches a raised object from a call to the __panic__ function. If a
panic is recovered from then the first, _on-recover_ form is evaluated for the return value
otherwise the rest of the forms are evaluated in order.`,
			Examples: []string{
				`(recover rec (7) (/ 1 0)) => 7`,
			},
		}, &Pkg)
}

// Recover represents the recover function.
type Recover struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Recover) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) < 2 {
		slip.PanicArgCount(f, 2, -1)
	}
	pos := len(args) - 1
	sym, ok := args[pos].(slip.Symbol)
	if !ok {
		slip.PanicType("symbol", args[pos], "symbol")
	}
	pos -= 2
	d2 := depth + 1
	defer func() {
		if rec := recover(); rec != nil {
			s2 := s.NewScope()
			s2.Let(sym, slip.SimpleObject(rec))
			form := args[len(args)-2]
			result = nil
			if form != nil {
				result = form.Eval(s2, d2)
			}
		}
	}()
	for ; 0 <= pos; pos-- {
		result = f.EvalArg(s, args, pos, d2)
	}
	return
}
