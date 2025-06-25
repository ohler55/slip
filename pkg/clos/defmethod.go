// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Defmethod{Function: slip.Function{Name: "defmethod", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "defmethod",
			Args: []*slip.DocArg{
				{
					Name: "method",
					Type: "list",
					Text: `Can be of two forms. If a list then the method being defined is for Flavors and
must be a list of the flavor name, optional method type, and operation (method name) If not a list then a
method name followed by method qualifiers will result in a CLOS method definition.`,
				},
				{
					Name: "args",
					Type: "lambda-list",
					Text: `The arguments to the method. A specialized lambda-list that allows providing
default values for each variable.`,
				},
				{Name: slip.AmpOptional},
				{
					Name: "documentation",
					Type: "string",
					Text: `Documentation for the method.`,
				},
				{Name: slip.AmpRest},
				{
					Name: "forms",
					Type: "object",
					Text: `The forms that process the method.`,
				},
			},
			Return: "nil",
			Text:   `__defmethod__ defines a method for a flavor or class.`,
			Examples: []string{
				"(defflavor strawberry (size) ()) => #<flavor strawberry>",
				`(defmethod (strawberry :before :size) () (format t "getting size"))  => nil`,
			},
		}, &Pkg)
}

// Defmethod represents the defmethod function.
type Defmethod struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Defmethod) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, -1)
	switch ta := args[0].(type) {
	case slip.Symbol:
		slip.NewPanic("Not yet implemented")
	case slip.List:
		flavors.DefmethodCall(s, ta, args[1:])
	default:
		slip.PanicType("method designator for defmethod", ta, "symbol", "list")
	}
	return
}
