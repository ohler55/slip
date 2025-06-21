// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Defpackage{Function: slip.Function{Name: "defpackage", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "defpackage",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "string|symbol",
					Text: "The name of the new package being defined.",
				},
				{Name: "&rest"},
				{
					Name: "options",
					Type: "list",
					Text: "A keyword followed by string values.",
				},
			},
			Return: "package",
			Text: `__defpackage__ Defines a new _package_ with the _name_ and options. Options are:
  :documentation
  :nicknames
  :use
  :export
  :shadow _(not yet supported)_
  :shadowing-import-from _(not yet supported)_
  :import-from _(not yet supported)_
`,
			Examples: []string{
				`(defpackage 'quux`,
				`  (:use "bag" "cl")`,
				`  (:nicknames "qux"")) => #<package quux>`,
			},
		}, &slip.CLPkg)
}

// Defpackage represents the defpackage function.
type Defpackage struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Defpackage) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 7)
	a0 := slip.EvalArg(s, args, 0, depth)
	name := slip.MustBeString(a0, "name")
	if slip.FindPackage(name) != nil {
		slip.NewPanic("Package %s already exists.", name)
	}
	rest := args[1:]
	nicknames := readDefOption(":nicknames", rest)
	for _, nn := range nicknames {
		if slip.FindPackage(nn) != nil {
			slip.NewPanic("Package %s already exists.", nn)
		}
	}
	var use []*slip.Package
	for _, name := range readDefOption(":use", rest) {
		if p := slip.FindPackage(name); p == nil {
			slip.PanicPackage(nil, "Package %s does not exist.", name)
		} else {
			use = append(use, p)
		}
	}
	// Check the other options for type correctness only.
	_ = readDefOption(":shadow", rest)
	_ = readDefOption(":shadowing-import-from", rest)
	_ = readDefOption(":import-from", rest)

	pkg := slip.DefPackage(name, nicknames, "")
	if docs := readDefOption(":documentation", rest); 0 < len(docs) {
		pkg.Doc = docs[0]
	}
	for _, p := range use {
		pkg.Use(p)
	}
	for _, str := range readDefOption(":export", rest) {
		pkg.Export(str)
	}
	return pkg
}

func readDefOption(name string, args slip.List) (values []string) {
	key := slip.Symbol(name)
	for _, arg := range args {
		option, ok := arg.(slip.List)
		if !ok || len(option) < 1 {
			slip.PanicType("option", arg, "list")
		}
		if option[0] == key {
			for _, v := range option[1:] {
				values = append(values, slip.MustBeString(v, "option"))
			}
			break
		}
	}
	return
}
