// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Unexport{Function: slip.Function{Name: "unexport", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "unexport",
			Args: []*slip.DocArg{
				{
					Name: "symbols",
					Type: "symbol|string|list",
					Text: "Symbols to unexport.",
				},
				{Name: "&optional"},
				{
					Name: "package",
					Type: "package",
					Text: "Package to unexport the _symbols_ from.",
				},
			},
			Text: `__unexport__ _symbols_ from _package_.`,
		}, &slip.CLPkg)
}

// Unexport represents the unexport function.
type Unexport struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Unexport) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 2)
	p := slip.CurrentPackage
	if 1 < len(args) {
		p = slip.PackageFromArg(args[1])
	}
	switch ta := args[0].(type) {
	case slip.Symbol:
		p.Unexport(string(ta))
	case slip.String:
		p.Unexport(string(ta))
	case slip.List:
		for _, v := range ta {
			switch tv := v.(type) {
			case slip.Symbol:
				p.Unexport(string(tv))
			case slip.String:
				p.Unexport(string(tv))
			default:
				slip.PanicType("symbol", tv, "symbol", "string")
			}
		}
	default:
		slip.PanicType("symbols", ta, "symbol", "string", "list")
	}
	return slip.True
}
