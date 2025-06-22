// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"sort"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := DoSymbols{Function: slip.Function{Name: "do-symbols", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "do-symbols",
			Args: []*slip.DocArg{
				{
					Name: "args",
					Type: "list",
					Text: "A list of a symbol and optional package and return form.",
				},
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "object",
					Text: "The forms to evaluate on each iteration.",
				},
			},
			Return: "list",
			Text: `__do-symbols__ expects the _args_ to be a list of a symbol which is not evaluated
and an optional package which is evaluated. An optional form can also be included in the _args_ that
is evaluated and the result returned. For each symbol in the package the _forms_ are called.`,
			Examples: []string{
				"(let ((lst ()))",
				" (do-symbols (s *watch* lst) (add lst s)) lst) => (watch-server watch-client ...)",
			},
		}, &slip.CLPkg)
}

// DoSymbols represents the do-symbols function.
type DoSymbols struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *DoSymbols) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, -1)
	d2 := depth + 1

	sargs, ok := args[0].(slip.List)
	if !ok {
		slip.PanicType("args", args[0], "list")
	}
	pkg := s.Get("*package*").(*slip.Package)
	var (
		sym   slip.Symbol
		rform slip.Object
	)
	if sym, ok = sargs[0].(slip.Symbol); !ok {
		slip.PanicType("args symbol", sargs[0], "symbol")
	}
	if 1 < len(sargs) {
		if pkg, ok = slip.EvalArg(s, sargs, 1, d2).(*slip.Package); !ok {
			slip.PanicType("args package", sargs[1], "package")
		}
		if 2 < len(sargs) {
			rform = sargs[2]
		}
	}
	// While not necessary all names are collected and sorted to make results
	// more consistent.
	nm := map[string]struct{}{}
	pkg.EachVarName(func(name string) { nm[name] = struct{}{} })
	pkg.EachFuncName(func(name string) { nm[name] = struct{}{} })
	for k, c := range slip.Constants {
		if c.Pkg == pkg { // TBD or uses
			nm[k] = struct{}{}
		}
		nm[k] = struct{}{} // TBD remove
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
