// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
	"sort"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FindAllSymbols{
				Function: slip.Function{Name: "find-all-symbols", Args: args},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "find-all-symbols",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string|symbol",
					Text: "The string designator to to compare to each symbol.",
				},
			},
			Return: "list",
			Text: `__find-all-symbols__ returns a list of all the symbols matching the _string_
across all packages.`,
			Examples: []string{
				`(intern "car" (make-package 'temp :use nil))`,
				`(find-all-symbols "car") => (car temp::car)`,
			},
		}, &slip.CLPkg)
}

// FindAllSymbols represents the find-all-symbols function.
type FindAllSymbols struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *FindAllSymbols) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	target := strings.ToLower(slip.MustBeString(args[0], "string"))
	var matches []string
	for _, p := range slip.AllPackages() {
		vv := p.GetVarVal(target)
		fi := p.GetFunc(target)
		if (vv == nil || vv.Pkg != p) && (fi == nil || fi.Pkg != p) {
			continue
		}
		vv = slip.CurrentPackage.GetVarVal(target)
		fi = slip.CurrentPackage.GetFunc(target)
		if (vv != nil && vv.Pkg == p) || (fi != nil && fi.Pkg == p) {
			matches = append(matches, target)
		} else {
			matches = append(matches, fmt.Sprintf("%s::%s", p.Name, target))
		}
	}
	sort.Strings(matches)
	syms := make(slip.List, len(matches))
	for i, m := range matches {
		syms[i] = slip.Symbol(m)
	}
	return syms
}
