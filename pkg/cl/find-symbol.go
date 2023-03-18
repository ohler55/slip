// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FindSymbol{Function: slip.Function{Name: "find-symbol", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "find-symbol",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "The _string_ to be searched for.",
				},
				{Name: "&optional"},
				{
					Name: "package",
					Type: "string|symbol|package",
					Text: "The package to find the symbol in. The default is the _*current-package*_.",
				},
			},
			Return: "symbol, status",
			Text: `__find-symbol__ Finds a _symbol_ matching _string_ in the _package_. The _symbol_ is
returned along with the _status_ where the _status_ can be one of:

  _:interal_ indicates the _symbol_ was found in the _package_.
  _:external_ indicates the _symbol_ was found in the _*keyword-package*_.
  _:inherited_ indicates the _symbol_ was found in one of the _use-package_ of the _package_.
  _nil_ indicates there was no pre-existing _symbol_.

`,
			Examples: []string{
				`(find-symbol "abc") => abc, nil`,
			},
		}, &slip.CLPkg)
}

// FindSymbol represents the findSymbol function.
type FindSymbol struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *FindSymbol) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) < 1 || 2 < len(args) {
		slip.PanicArgCount(f, 1, 2)
	}
	so, ok := args[0].(slip.String)
	if !ok || len(so) < 1 {
		slip.PanicType("string", args[0], "string")
	}
	p := slip.CurrentPackage
	if 1 < len(args) {
		switch ta := args[1].(type) {
		case slip.Symbol:
			if p = slip.FindPackage(string(ta)); p == nil {
				panic(fmt.Sprintf("package %s not found", ta))
			}
		case slip.String:
			if p = slip.FindPackage(string(ta)); p == nil {
				panic(fmt.Sprintf("package %s not found", ta))
			}
		case *slip.Package:
			p = ta
		default:
			slip.PanicType("package", ta, "package", "string", "symbol")
		}
	}
	if p == &slip.KeywordPkg && so[0] != ':' {
		so = slip.String(":") + so
	}
	var status slip.Object
	vv := p.GetVarVal(string(so))
	switch {
	case vv == nil:
		return slip.Values{nil, nil}
	case vv.Pkg == &slip.KeywordPkg:
		status = slip.Symbol(":external")
	case vv.Pkg == p:
		status = slip.Symbol(":internal")
	default:
		status = slip.Symbol(":inherited")
	}
	return slip.Values{slip.Symbol(so), status}
}
