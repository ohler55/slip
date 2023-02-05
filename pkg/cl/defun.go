// Copyright (c) 2022, Peter Ohler, All rights reserved.

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
			f := Defun{Function: slip.Function{Name: "defun", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
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

// Call the the function with the arguments provided.
func (f *Defun) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	name, ok := args[len(args)-1].(slip.Symbol)
	if !ok {
		slip.PanicType("name argument to defun", args[len(args)-1], "symbol")
	}
	low := strings.ToLower(string(name))
	lc := slip.DefLambda("defun", s, args[:len(args)-1])
	fc := func(fargs slip.List) slip.Object {
		return &slip.Dynamic{
			Function: slip.Function{
				Name: low,
				Self: lc,
				Args: fargs,
			},
		}
	}
	if fi := slip.CurrentPackage.Funcs[low]; fi != nil {
		if fi.Pkg.Locked {
			panic(fmt.Sprintf("Redefining %s:%s in defun. Package %s is locked.",
				slip.CurrentPackage.Name, low, slip.CurrentPackage.Name))
		}
		var w io.Writer
		if w, ok = slip.ErrorOutput.(io.Writer); ok {
			_, _ = fmt.Fprintf(w, "WARNING: redefining %s:%s in defun\n", slip.CurrentPackage.Name, low)
		}
	}
	slip.CurrentPackage.Lambdas[low] = lc
	slip.CurrentPackage.Funcs[low] = &slip.FuncInfo{Name: low, Doc: lc.Doc, Create: fc, Pkg: slip.CurrentPackage}
	if s.Parent != nil {
		lc.Closure = s
	}
	return name
}
