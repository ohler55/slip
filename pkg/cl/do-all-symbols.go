// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"sort"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := DoAllSymbols{Function: slip.Function{Name: "do-all-symbols", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "do-all-symbols",
			Args: []*slip.DocArg{
				{
					Name: "args",
					Type: "list",
					Text: "A list of a symbol and optional return form.",
				},
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "object",
					Text: "The forms to evaluate on each iteration.",
				},
			},
			Return: "list",
			Text: `__do-all-symbols__ expects the _args_ to be a list of a symbol which is not evaluated.
An optional form can also be included in the _args_ that is evaluated and the result returned. For
each symbol in all the packages the _forms_ are called.`,
			Examples: []string{
				"(let ((lst ()))",
				" (do-all-symbols (s lst) (add lst s))) => (car cdr ...)",
			},
		}, &slip.CLPkg)
}

// DoAllSymbols represents the do-all-symbols function.
type DoAllSymbols struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *DoAllSymbols) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, -1)
	d2 := depth + 1

	sargs, ok := args[0].(slip.List)
	if !ok {
		slip.PanicType("args", args[0], "list")
	}
	var (
		sym   slip.Symbol
		rform slip.Object
	)
	if sym, ok = sargs[0].(slip.Symbol); !ok {
		slip.PanicType("args symbol", sargs[0], "symbol")
	}
	if 1 < len(sargs) {
		rform = sargs[1]
	}
	// While not necessary all names are collected and sorted to make results
	// more consistent.
	nm := map[string]struct{}{}
	for _, pkg := range slip.AllPackages() {
		pkg.EachVarName(func(name string) { nm[name] = struct{}{} })
		pkg.EachFuncName(func(name string) { nm[name] = struct{}{} })
	}
	for k := range slip.Constants {
		nm[k] = struct{}{}
	}
	names := make([]string, 0, len(nm))
	for name := range nm {
		names = append(names, name)
	}
	sort.Strings(names)

	ss := s.NewScope()
	ss.Block = true
	forms := args[1:]
	for _, name := range names {
		ss.Let(sym, slip.Symbol(name))
		for i := range forms {
			if rr, ok2 := slip.EvalArg(ss, forms, i, d2).(*slip.ReturnResult); ok2 {
				if rr.Tag == nil {
					return rr.Result
				}
				return rr
			}
		}
	}
	return ss.Eval(rform, d2)
}
