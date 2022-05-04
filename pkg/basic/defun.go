// Copyright (c) 2022, Peter Ohler, All rights reserved.

package basic

import (
	"fmt"
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
			Text:   `defines a function with the given _name_ in the current package.`,
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
	pos := len(args) - 1
	name, ok := args[pos].(slip.Symbol)
	if !ok {
		slip.PanicType("name argument to defun", args[pos], "symbol")
	}
	upper := strings.ToUpper(string(name))
	pos--
	var ll slip.List
	if ll, ok = args[pos].(slip.List); !ok {
		slip.PanicType("lambda list of defun", args[pos], "list")
	}
	pos--
	var docStr slip.String
	if docStr, ok = args[pos].(slip.String); !ok {
		pos++
	}
	var lc *slip.LispCaller
	if fi := slip.CurrentPackage.Funcs[upper]; fi == nil {
		lc = &slip.LispCaller{
			Name: upper,
			Doc: &slip.FuncDoc{
				Name:   upper,
				Return: "object",
				Text:   string(docStr),
			},
			Forms: args[:pos],
		}
		fc := func(args slip.List) slip.Object {
			return &slip.Dynamic{
				Function: slip.Function{
					Name: upper,
					Self: lc,
				},
			}
		}
		slip.CurrentPackage.LispCallers[upper] = lc
		fi = &slip.FuncInfo{Create: fc, Pkg: slip.CurrentPackage}
		slip.CurrentPackage.Funcs[upper] = fi
	} else {
		lc = slip.CurrentPackage.LispCallers[upper]
	}
	if s.Parent() != nil {
		lc.Closure = s
	}

	// TBD build doc args
	// lc.Doc.Args     []*DocArg

	fmt.Printf("*** defun %s %s docs: %q forms: %s\n", lc.Name, ll, lc.Doc.Text, lc.Forms)

	return name
}
