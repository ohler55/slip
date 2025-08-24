// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
	"io"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Defmacro{Function: slip.Function{Name: "defmacro", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "defmacro",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "symbol",
					Text: "The name of the function being defined.",
				},
				{
					Name: "lambda-list",
					Type: "list",
					Text: `A list of the macro's argument. Arguments can be a symbol, a list of a symbol and a
default value, or one of &optional, &rest, &key, or &body. Arguments declarations following &optional are optional.
Argument declarations following &key represent key values. An argument declaration following &rest or &body indicates
one or more values follow.`,
				},
				{Name: slip.AmpOptional},
				{
					Name: "documentation",
					Type: "string",
					Text: "Documentation for the macro.",
				},
				{Name: slip.AmpBody},
				{
					Name: "forms",
					Type: "object",
					Text: "The objects/forms to evaluate when the function is called.",
				},
			},
			Return: "object",
			Text:   `__defmacro__ defines a macro with the given _name_ in the current package.`,
			Examples: []string{
				"(defmacro funny (x) `(* ,x 2)) => funny",
			},
		}, &slip.CLPkg)
}

// Defmacro represents the defmacro function.
type Defmacro struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Defmacro) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	name, ok := args[0].(slip.Symbol)
	if !ok {
		slip.TypePanic(s, depth, "name argument to defmacro", args[0], "symbol")
	}
	low := strings.ToLower(string(name))
	lc := slip.DefLambda("defmacro", s, args[1:])
	lc.Macro = true
	fc := func(fargs slip.List) slip.Object {
		return &slip.Dynamic{
			Function: slip.Function{
				Name:     low,
				Self:     lc,
				Args:     fargs,
				SkipEval: []bool{true},
			},
		}
	}
	if fi := slip.CurrentPackage.GetFunc(low); fi != nil {
		if fi.Pkg.Locked {
			slip.PackagePanic(s, depth, slip.CurrentPackage, "Redefining %s:%s in defmacro. Package %s is locked.",
				slip.CurrentPackage.Name, low, slip.CurrentPackage.Name)
		}
		if 0 < len(fi.Kind) {
			w := s.Get("*error-output*").(io.Writer)
			_, _ = fmt.Fprintf(w, "WARNING: redefining %s:%s in defmacro\n", slip.CurrentPackage.Name, low)
		}
	}
	slip.CurrentPackage.DefLambda(low, lc, fc, slip.MacroSymbol)
	if 0 < len(s.Parents()) {
		lc.Closure = s
	}
	return name
}
