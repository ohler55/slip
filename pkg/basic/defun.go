// Copyright (c) 2022, Peter Ohler, All rights reserved.

package basic

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
	switch tl := args[pos].(type) {
	case slip.List:
		ll = tl
	case nil:
		// leave as empty list
	default:
		slip.PanicType("lambda list of defun", args[pos], "list")
	}
	pos--
	var docStr slip.String
	if docStr, ok = args[pos].(slip.String); !ok {
		pos++
	}
	lc := &slip.LispCaller{
		Name: upper,
		Doc: &slip.FuncDoc{
			Name:   upper,
			Return: "object",
			Text:   string(docStr),
		},
		Forms: args[:pos],
	}
	fc := func(fargs slip.List) slip.Object {
		return &slip.Dynamic{
			Function: slip.Function{
				Name: upper,
				Self: lc,
				Args: fargs,
			},
		}
	}
	for i := len(ll) - 1; 0 <= i; i-- {
		switch ta := ll[i].(type) {
		case slip.Symbol: // variable name
			lc.Doc.Args = append(lc.Doc.Args, &slip.DocArg{Name: string(ta), Type: "object"})
		case slip.List: // variable name and default value
			if len(ta) != 2 {
				slip.PanicType("lambda list element with default value", ta, "list of two elements")
			}
			if name, ok = ta[1].(slip.Symbol); !ok {
				slip.PanicType("lambda list element with default value", ta, "list with a symbol as the first element")
			}
			if ta[0] == nil {
				lc.Doc.Args = append(lc.Doc.Args, &slip.DocArg{Name: string(name), Type: "object"})
			} else {
				lc.Doc.Args = append(lc.Doc.Args,
					&slip.DocArg{Name: string(name), Type: string(ta[0].Hierarchy()[0]), Default: ta[0]})
			}
		default:
			slip.PanicType("lambda list element", ta, "symbol", "list")
		}
	}
	if fi := slip.CurrentPackage.Funcs[upper]; fi != nil {
		if fi.Pkg.Locked {
			panic(fmt.Sprintf("Redefining %s::%s in DEFUN. Package %s is locked.",
				slip.CurrentPackage.Name, upper, slip.CurrentPackage.Name))
		}
		var w io.Writer
		if w, ok = slip.ErrorOutput.(io.Writer); ok {
			_, _ = fmt.Fprintf(w, "WARNING: redefining %s::%s in DEFUN\n", slip.CurrentPackage.Name, upper)
		}
	}
	slip.CurrentPackage.LispCallers[upper] = lc
	slip.CurrentPackage.Funcs[upper] = &slip.FuncInfo{Create: fc, Pkg: slip.CurrentPackage}
	if s.Parent() != nil {
		lc.Closure = s
	}
	return name
}
