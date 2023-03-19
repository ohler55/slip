// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Defparameter{
				Function: slip.Function{Name: "defparameter", Args: args, SkipEval: []bool{true, false, true}},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "defparameter",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "symbol",
					Text: "The name of the function being defined.",
				},
				{Name: slip.AmpOptional},
				{
					Name: "initial-value",
					Type: "object",
					Text: "Initial value for the variable.",
				},
				{
					Name: "documentation",
					Type: "string",
					Text: "Documentation for the variable.",
				},
			},
			Return: "symbol",
			Text: `__defparameter__ defines a variable with the given _name_ in the current package and
binds _name_ to the _initial-value_. If _name_ is already defined the variable value is still bound
to _initial-value_.`,
			Examples: []string{
				"(defparameter v 7) => v",
			},
		}, &slip.CLPkg)
}

// Defparameter represents the defparameter function.
type Defparameter struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Defparameter) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) < 1 || 3 < len(args) {
		slip.PanicArgCount(f, 1, 3)
	}
	name, ok := args[0].(slip.Symbol)
	if !ok {
		slip.PanicType("name argument to defparameter", args[0], "symbol")
	}
	name = slip.Symbol(strings.ToLower(string(name)))
	var (
		iv  slip.Object
		doc slip.String
	)
	if 1 < len(args) {
		iv = args[1]
		if 2 < len(args) {
			var ok bool
			if doc, ok = args[2].(slip.String); !ok {
				slip.PanicType("documentation argument to defparameter", args[2], "string")
			}
		}
	}
	vv := slip.CurrentPackage.Set(string(name), iv)
	if 0 < len(doc) {
		vv.Doc = string(doc)
	}
	return name
}
