// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Defconstant{Function: slip.Function{Name: "defconstant", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "defconstant",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "symbol",
					Text: "The name of the function being defined.",
				},
				{
					Name: "initial-value",
					Type: "object",
					Text: "Initial value for the variable.",
				},
				{Name: slip.AmpOptional},
				{
					Name: "documentation",
					Type: "string",
					Text: "Documentation for the variable.",
				},
			},
			Return: "symbol",
			Text: `__defconstant__ defines a constant with the given _name_ and
binds _name_ to the _initial-value_. If _name_ is already defined with a different
initial value the call panics.`,
			Examples: []string{
				"(defconstant global 7) => global",
			},
		}, &slip.CLPkg)
}

// Defconstant represents the defconstant function.
type Defconstant struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Defconstant) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 3)
	sym, ok := args[0].(slip.Symbol)
	if !ok {
		slip.TypePanic(s, depth, "name argument to defconstant", args[0], "symbol")
	}
	name := strings.ToLower(string(sym))
	pkg, vname, _ := slip.UnpackName(name)
	if pkg == nil {
		pkg = slip.CurrentPackage
	}
	if pkg.Locked {
		slip.PanicPackage(pkg, "Redefining %s:%s constant. Package %s is locked.",
			pkg.Name, vname, pkg.Name)
	}
	var doc slip.String
	iv := slip.EvalArg(s, args, 1, depth+1)
	if 2 < len(args) {
		var ok bool
		if doc, ok = args[2].(slip.String); !ok {
			slip.TypePanic(s, depth, "documentation argument to defconstant", args[2], "string")
		}
	}
	pkg.DefConst(vname, iv, string(doc))

	return slip.Symbol(vname)
}
