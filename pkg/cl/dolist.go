// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Dolist{Function: slip.Function{Name: "dolist", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "dolist",
			Args: []*slip.DocArg{
				{
					Name: "input",
					Type: "list",
					Text: "A list of _var_, _list-form_, and optionally _result-form_.",
				},
				{Name: "&rest"},
				{
					Name: "body",
					Type: "statement|tag",
					Text: "The forms to evaluate on each iteration.",
				},
			},
			Return: "object",
			Text: `__dolist__ iterates over the _body_ evaluating each form with _var_
bound to each element in the _list-form_ evaluation result. After the last element is
processed _var_ is bound to _nil_ and the _result-form_ is evaluated as the result of
the __dolist__. The __dolist__ allows for __return__ and __go__ forms in the body.`,
			Examples: []string{
				"(let ((sum 0)) (dolist (x '(1 2 3) sum) (setq sum (+ sum x)))) => 6",
			},
		}, &slip.CLPkg)
}

// Dolist represents the dolist function.
type Dolist struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Dolist) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, -1)
	ns := s.NewScope()
	ns.Block = true
	ns.TagBody = true
	d2 := depth + 1
	var (
		sym   slip.Symbol
		list  slip.List
		rform slip.Object
	)
	if input, ok := args[0].(slip.List); ok && 2 <= len(input) {
		if sym, ok = input[0].(slip.Symbol); !ok {
			slip.PanicType("dolist input var", input[0], "symbol")
		}
		sym = slip.Symbol(strings.ToLower(string(sym)))
		switch t1 := ns.Eval(input[1], d2).(type) {
		case nil:
			// leave list as empty list
		case slip.List:
			list = t1
		default:
			slip.PanicType("dolist input list", t1, "list")
		}
		if 2 < len(input) {
			rform = input[2]
			if 3 < len(input) {
				slip.NewPanic("too many elements in %s", input)
			}
		}
	} else {
		slip.PanicType("dolist input", args[0], "list")
	}
	ns.Let(sym, nil) // use the safe way to verify it's a valid symbol to use for a let.
	for _, v := range list {
		ns.UnsafeLet(sym, v)
		for i := 1; i < len(args); i++ {
			switch args[i].(type) {
			case slip.List, slip.Funky:
				switch tr := slip.EvalArg(ns, args, i, d2).(type) {
				case *slip.ReturnResult:
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
	ns.UnsafeLet(sym, nil)

	return ns.Eval(rform, d2)
}
