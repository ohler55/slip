// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"

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
					Text: "A list of a symbol and optional package.",
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
				" (do-symbols (s *watch* lst) (add lst s))) => (watch-server watch-client ...)",
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
	fmt.Printf("*** %s\n", sargs)
	fmt.Printf("*** sym: %s\n", sym)
	fmt.Printf("*** pkg: %s\n", pkg)
	fmt.Printf("*** caller: %s\n", rform)

	pkg.EachVarName(
		func(name string) {
			// TBD
			fmt.Printf("*** %s\n", name)
		},
	)
	pkg.EachFuncName(
		func(name string) {
			// TBD
			fmt.Printf("*** %s\n", name)
		},
	)

	// for i := range args {
	// 	if result = slip.EvalArg(s, args, i, d2); result == nil {
	// 		break
	// 	}
	// }

	return nil
}

// (let ((lst ())) (do-external-symbols (s (find-package 'sb-gray) lst) (push s lst)))
