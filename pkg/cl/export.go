// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Export{Function: slip.Function{Name: "export", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "export",
			Args: []*slip.DocArg{
				{
					Name: "symbols",
					Type: "symbol|string|list",
					Text: "Symbols to export.",
				},
				{Name: "&optional"},
				{
					Name: "package",
					Type: "package",
					Text: "Package to export the _symbols_ from.",
				},
			},
			Text: `__export__ _symbols_ from _package_.`,
		}, &slip.CLPkg)
}

// Export represents the export function.
type Export struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Export) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 2)
	p := slip.CurrentPackage
	if 1 < len(args) {
		p = packageFromArg(args[1], "export")
	}
	switch ta := args[0].(type) {
	case slip.Symbol:
		p.Export(string(ta))
	case slip.String:
		p.Export(string(ta))
	case slip.List:
		for _, v := range ta {
			switch tv := v.(type) {
			case slip.Symbol:
				p.Export(string(tv))
			case slip.String:
				p.Export(string(tv))
			default:
				slip.PanicType("symbol", tv, "symbol", "string")
			}
		}
	default:
		slip.PanicType("symbols", ta, "symbol", "string", "list")
	}
	return slip.True
}
