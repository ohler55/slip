// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Remf{Function: slip.Function{Name: "remf", Args: args, SkipEval: []bool{true, false}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "remf",
			Args: []*slip.DocArg{
				{
					Name: "place",
					Type: "place",
					Text: "The symbol or place to remove a property from.",
				},
				{
					Name: "indicator",
					Type: "object",
					Text: "An indicator of the property to remove.",
				},
			},
			Return: "bool",
			Text:   `__remf__ removes the indicated property from _place_.`,
			Examples: []string{
				"(setq quux '(a 1 b 2 c 3))",
				"(remf quux 'b) => t ;; quux is now (a 1 c 3)",
				"(remf quux 'b) => nil",
			},
		}, &slip.CLPkg)
}

// Remf represents the remf function.
type Remf struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Remf) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 2)
	d2 := depth + 1
	var plist slip.List
	switch ta := s.Eval(args[0], d2).(type) {
	case nil:
		// leave as an empty list
	case slip.List:
		plist = ta
	default:
		slip.PanicType("plist", args[0], "property list")
	}
	ind := args[1]
	for i := 0; i < len(plist)-1; i += 2 {
		if slip.ObjectEqual(ind, plist[i]) {
			result = slip.True
			plist = append(plist[:i], plist[i+2:]...)
			break
		}
	}
	p := args[0]
Retry:
	switch ta := p.(type) {
	case slip.Symbol:
		s.Set(ta, plist)
	case slip.List:
		p = slip.ListToFunc(s, ta, d2)
		goto Retry
	case slip.Placer:
		callPlace(s, ta, plist, d2)
	default:
		slip.PanicType("place", p, "symbol", "placer")
	}
	return
}
