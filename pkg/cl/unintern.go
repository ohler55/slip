// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Unintern{Function: slip.Function{Name: "unintern", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "unintern",
			Args: []*slip.DocArg{
				{
					Name: "symbol",
					Type: "symbol",
					Text: "The _symbol_ to be uninterned.",
				},
				{Name: "&optional"},
				{
					Name: "package",
					Type: "string|symbol|package",
					Text: "The package to unintern the symbol from. The default is the _*current-package*_.",
				},
			},
			Return: "boolean",
			Text: `__unintern__ Unbinds the _symbol_ in the _package_. If found and unbound _t_ is
returned otherwise _nil_ is returned.
`,
			Examples: []string{
				`(unintern 'abc) => t`,
			},
		}, &slip.CLPkg)
}

// Unintern represents the unintern function.
type Unintern struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Unintern) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) < 1 || 2 < len(args) {
		slip.PanicArgCount(f, 1, 2)
	}
	so, ok := args[len(args)-1].(slip.Symbol)
	if !ok || len(so) < 1 {
		slip.PanicType("symbol", args[len(args)-1], "symbol")
	}
	p := slip.CurrentPackage
	if 1 < len(args) {
		switch ta := args[0].(type) {
		case slip.Symbol:
			if p = slip.FindPackage(string(ta)); p == nil {
				panic(fmt.Sprintf("package %s not found", ta))
			}
		case slip.String:
			if p = slip.FindPackage(string(ta)); p == nil {
				panic(fmt.Sprintf("package %s not found", ta))
			}
		case *slip.Package:
			p = ta
		default:
			slip.PanicType("package", args[0], "package", "string", "symbol")
		}
	}
	if p == &slip.KeywordPkg && so[0] != ':' {
		so = slip.Symbol(":") + so
	}
	if p.Remove(string(so)) {
		return slip.True
	}
	return nil
}
