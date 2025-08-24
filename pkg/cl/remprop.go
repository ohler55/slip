// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Remprop{Function: slip.Function{Name: "remprop", Args: args, SkipEval: []bool{true, false}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "remprop",
			Args: []*slip.DocArg{
				{
					Name: "symbol",
					Type: "symbol",
					Text: "The symbol to a property from.",
				},
				{
					Name: "indicator",
					Type: "object",
					Text: "An indicator of the property to remove.",
				},
			},
			Return: "bool",
			Text:   `__remprop__ removes the indicated property from the property list bound to _symbol_.`,
			Examples: []string{
				"(setq quux '(a 1 b 2 c 3))",
				"(remprop quux 'b) => t ;; quux is now (a 1 c 3)",
				"(remprop quux 'b) => nil",
			},
		}, &slip.CLPkg)
}

// Remprop represents the remprop function.
type Remprop struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Remprop) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	sym, ok := args[0].(slip.Symbol)
	if !ok {
		slip.TypePanic(s, depth, "symbol", args[0], "symbol")
	}
	var plist slip.List
	switch ta := s.Get(sym).(type) {
	case nil:
		// leave as an empty list
	case slip.List:
		plist = ta
	default:
		slip.TypePanic(s, depth, "plist", args[0], "property list")
	}
	ind := args[1]
	for i := 0; i < len(plist)-1; i += 2 {
		if slip.ObjectEqual(ind, plist[i]) {
			result = slip.True
			plist = append(plist[:i], plist[i+2:]...)
			break
		}
	}
	s.Set(sym, plist)

	return
}
