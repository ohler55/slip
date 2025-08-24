// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Get{Function: slip.Function{Name: "get", Args: args, SkipEval: []bool{true, false}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "get",
			Args: []*slip.DocArg{
				{
					Name: "symbol",
					Type: "symbol",
					Text: "A symbol bound to a property list to lookup a value in.",
				},
				{
					Name: "indicator",
					Type: "object",
					Text: "The indicator for a property.",
				},
				{Name: "&optional"},
				{
					Name: "default",
					Type: "object",
					Text: `The default value to return if the indicator does not identify a value.`,
				},
			},
			Return: "object",
			Text:   `__get__ returns the value associated with the _indicator_.`,
			Examples: []string{
				"(get '(a 1 b 2 c 3) 'b) => 2",
				"(setq quux '(a 1 b 2 c 3))",
				"(setf (get quux 'b) 4)",
				"quux => (a 1 b 4 c 3)",
			},
		}, &slip.CLPkg)
}

// Get represents the get function.
type Get struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Get) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 3)
	var plist slip.List
	switch ta := s.Eval(args[0], depth+1).(type) {
	case nil:
		// leave as an empty list
	case slip.List:
		plist = ta
	default:
		slip.TypePanic(s, depth, "symbol", args[0], "symbol bound to a property list")
	}
	ind := args[1]
	if 2 < len(args) {
		result = args[2]
	}
	for i := 0; i < len(plist)-1; i += 2 {
		if slip.ObjectEqual(ind, plist[i]) {
			return plist[i+1]
		}
	}
	return
}

// Place a value in the plist.
func (f *Get) Place(s *slip.Scope, args slip.List, value slip.Object) {
	slip.CheckArgCount(s, 0, f, args, 2, 3)
	var plist slip.List
	switch ta := s.Eval(args[0], 0).(type) {
	case nil:
		// leave as an empty list
	case slip.List:
		plist = ta
	default:
		slip.TypePanic(s, 0, "plist", args[0], "property list")
	}
	ind := args[1]
	for i := 0; i < len(plist)-1; i += 2 {
		if slip.ObjectEqual(ind, plist[i]) {
			plist[i+1] = value
			return
		}
	}
	plist = append(plist, args[1], value)
	if sym, ok := args[0].(slip.Symbol); ok {
		s.Set(sym, plist)
	} else {
		slip.TypePanic(s, 0, "symbol", args[0], "symbol")
	}
}
