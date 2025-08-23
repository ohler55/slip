// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Defun{Function: slip.Function{Name: "defun", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "defun",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "symbol",
					Text: "The name of the function being defined.",
				},
				{
					Name: "lambda-list",
					Type: "list",
					Text: `A list of the function's argument. Arguments can be a symbol, a list of a symbol and a
default value, or one of &optional, &rest, &key, or &body. Arguments declarations following &optional are optional.
Argument declarations following &key represent key values. An argument declaration following &rest or &body indicates
one or more values follow.`,
				},
				{Name: slip.AmpOptional},
				{
					Name: "documentation",
					Type: "string",
					Text: "Documentation for the function.",
				},
				{Name: slip.AmpBody},
				{
					Name: "forms",
					Type: "object",
					Text: "The objects/forms to evaluate when the function is called.",
				},
			},
			Return: "object",
			Text:   `__defun__ defines a function with the given _name_ in the current package.`,
			Examples: []string{
				"(defun funny () nil) => funny",
			},
		}, &slip.CLPkg)
}

// Defun represents the defun function.
type Defun struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Defun) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	name, ok := args[0].(slip.Symbol)
	if !ok {
		slip.TypePanic(s, depth, "name argument to defun", args[0], "symbol")
	}
	pkg, low, _ := slip.UnpackName(string(name))
	if pkg == nil {
		pkg = slip.CurrentPackage
	}
	lc := slip.DefLambda(low, s, args[1:])
	fc := func(fargs slip.List) slip.Object {
		return &slip.Dynamic{
			Function: slip.Function{
				Name: low,
				Self: lc,
				Args: fargs,
			},
		}
	}
	if fi := pkg.GetFunc(low); fi != nil {
		if fi.Pkg.Locked {
			slip.PackagePanic(s, depth, fi.Pkg, "Redefining %s:%s in defun. Package %s is locked.",
				fi.Pkg.Name, low, fi.Pkg.Name)
		}
		if 0 < len(fi.Kind) {
			w := s.Get("*error-output*").(io.Writer)
			_, _ = fmt.Fprintf(w, "WARNING: redefining %s:%s in defun\n", slip.CurrentPackage.Name, low)
		}
	}
	pkg.DefLambda(low, lc, fc, slip.FunctionSymbol)
	if 0 < len(s.Parents()) {
		lc.Closure = s
	}
	return name
}
