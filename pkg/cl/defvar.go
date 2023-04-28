// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Defvar{Function: slip.Function{Name: "defvar", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "defvar",
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
			Text: `__defvar__ defines a variable with the given _name_ in the current package and
binds _name_ to the _initial-value_. If _name_ is already defined the variable value is not changed.`,
			Examples: []string{
				"(defvar v 7) => v",
			},
		}, &slip.CLPkg)
}

// Defvar represents the defvar function.
type Defvar struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Defvar) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 3)
	name, ok := args[0].(slip.Symbol)
	if !ok {
		slip.PanicType("name argument to defvar", args[0], "symbol")
	}
	name = slip.Symbol(strings.ToLower(string(name)))
	if v, has := slip.CurrentPackage.Get(string(name)); has && v != slip.Unbound {
		return name
	}
	var (
		iv  slip.Object = slip.Unbound
		doc slip.String
	)
	if 1 < len(args) {
		iv = slip.EvalArg(s, args, 1, depth+1)
		if 2 < len(args) {
			var ok bool
			if doc, ok = args[2].(slip.String); !ok {
				slip.PanicType("documentation argument to defvar", args[2], "string")
			}
		}
	}
	vv := slip.CurrentPackage.Set(string(name), iv)
	vv.Doc = string(doc)

	return name
}
