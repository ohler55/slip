// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Dotimes{Function: slip.Function{Name: "dotimes", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "dotimes",
			Args: []*slip.DocArg{
				{
					Name: "input",
					Type: "list",
					Text: "A list of _var_, _count-form_, and optionally _result-form_.",
				},
				{Name: "&rest"},
				{
					Name: "body",
					Type: "statement|tag",
					Text: "The forms to evaluate on each iteration.",
				},
			},
			Return: "object",
			Text: `__dotimes__ iterates over the _body_ evaluating each form with _var_
bound to a _integer_ from 0 to one less than the value returned by _count-form_. After
the last element is processed _var_ is bound to value returned by _count-form_ and the
_result-form_ is evaluated as the result of the __dotimes__. The __dotimes__ allows for
__return__ and __go__ forms in the body.`,
			Examples: []string{
				"(dotimes (x 3 (* x 2))) => 6",
			},
		}, &slip.CLPkg)
}

// Dotimes represents the dotimes function.
type Dotimes struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Dotimes) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, -1)
	ns := s.NewScope()
	ns.Block = true
	ns.TagBody = true
	d2 := depth + 1
	var (
		sym   slip.Symbol
		max   int64
		rform slip.Object
	)
	if input, ok := args[0].(slip.List); ok && 2 <= len(input) {
		if sym, ok = input[0].(slip.Symbol); !ok {
			slip.PanicType("dotimes input var", input[0], "symbol")
		}
		sym = slip.Symbol(strings.ToLower(string(sym)))
		if i, ok2 := ns.Eval(input[1], d2).(slip.Integer); ok2 {
			max = i.Int64()
		} else {
			slip.PanicType("dotimes input count", input[1], "integer")
		}
		if 2 < len(input) {
			rform = input[2]
			if 3 < len(input) {
				slip.NewPanic("too many elements in %s", input)
			}
		}
	} else {
		slip.PanicType("dotimes input", args[0], "list")
	}
	ns.Let(sym, nil) // use the safe way to verify it's a valid symbol to use for a let.
	for i := int64(0); i < max; i++ {
		ns.UnsafeLet(sym, slip.Fixnum(i))
		for i := 1; i < len(args); i++ {
			switch args[i].(type) {
			case slip.List, slip.Funky:
				switch tr := slip.EvalArg(ns, args, i, d2).(type) {
				case *ReturnResult:
					if tr.Tag == nil {
						return tr.Result
					}
					return tr
				case *GoTo:
					for i++; i < len(args); i++ {
						if args[i] == tr.Tag {
							break
						}
					}
				}
			}
		}
	}
	ns.UnsafeLet(sym, slip.Fixnum(max))

	return ns.Eval(rform, d2)
}
