// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Getf{Function: slip.Function{Name: "getf", Args: args, SkipEval: []bool{true, false}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "getf",
			Args: []*slip.DocArg{
				{
					Name: "plist",
					Type: "property list",
					Text: "The property list to lookup a value in.",
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
			Text:   `__getf__ returns the value associated with the _indicator_.`,
			Examples: []string{
				"(getf '(a 1 b 2 c 3) 'b) => 2",
				"(setq quux '(a 1 b 2 c 3))",
				"(setf (getf quux 'b) 4)",
				"quux => (a 1 b 4 c 3)",
			},
		}, &slip.CLPkg)
}

// Getf represents the getf function.
type Getf struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Getf) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 3)
	var plist slip.List
	switch ta := s.Eval(args[0], depth+1).(type) {
	case nil:
		// leave as an empty list
	case slip.List:
		plist = ta
	default:
		slip.PanicType("plist", args[0], "property list")
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
func (f *Getf) Place(s *slip.Scope, args slip.List, value slip.Object) {
	slip.ArgCountCheck(f, args, 2, 3)
	var plist slip.List
	switch ta := s.Eval(args[0], 0).(type) {
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
			plist[i+1] = value
			return
		}
	}
	plist = append(plist, args[1], value)
	switch ta := args[0].(type) {
	case slip.Symbol:
		s.Set(ta, plist)
	case slip.Placer:

		// TBD

	default:
		slip.PanicType("plist", args[0], "property list")
	}
}
