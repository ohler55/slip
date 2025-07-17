// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeCondition{Function: slip.Function{Name: "make-condition", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-condition",
			Args: []*slip.DocArg{
				{
					Name: "type",
					Type: "symbol",
					Text: "The type name to make.",
				},
				{Name: "&rest"},
				{
					Name: "initializers",
					Type: "keyword/value pairs",
					Text: `The slot initializers as keyword value pairs.`,
				},
			},
			Return: "condition",
			Text: `__make-condition__ makes a _condition_ of the _type_ specified initialized with the
values from the _initializers_ list.`,
			Examples: []string{
				"(make-condition 'unbound-slot :name 'slop) => #<UNBOUND-SLOT 12345>",
			},
		}, &slip.CLPkg)
}

// MakeCondition represents the make-condition function.
type MakeCondition struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeCondition) Call(s *slip.Scope, args slip.List, depth int) (cond slip.Object) {
	slip.ArgCountCheck(f, args, 1, -1)
	sym, ok := args[0].(slip.Symbol)
	if !ok {
		slip.PanicType("type", args[0], "symbol")
	}
	if c := slip.FindClass(string(sym)); c != nil && c.Metaclass() == slip.Symbol("condition-class") {
		obj := c.MakeInstance()
		obj.Init(s, args[1:], depth+1)
		return obj
	}
	slip.NewPanic("%s does not designate a condition class.", sym)

	return
}
