// Copyright (c) 2022, Peter Ohler, All rights reserved.

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
			f := Apropos{Function: slip.Function{Name: "apropos", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "apropos",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string|symbol",
					Text: "A pattern to search for across symbols and functions.",
				},
				{Name: "&optional"},
				{
					Name: "package",
					Type: "string|symbol",
					Text: "A package to limit the search to.",
				},
			},
			Return: "nil",
			Text: `Search all symbols for a symbols that contains the provided string or symbol.
The matches are printed to *standard-output* along with the package they are from and what the
symbol is associated with.`,
			Examples: []string{
				`(apropos "terpri") => nil ;; terpri (builtin) is written`,
			},
		}, &slip.CLPkg)
}

// Apropos represents the apropos function.
type Apropos struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Apropos) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) < 1 || 2 < len(args) {
		slip.PanicArgCount(f, 1, 2)
	}
	var pat string
	switch ta := args[len(args)-1].(type) {
	case slip.Symbol:
		pat = strings.ToLower(string(ta))
	case slip.String:
		pat = strings.ToLower(string(ta))
	default:
		slip.PanicType("string", ta, "string", "symbol")
	}
	var pkg *slip.Package
	if 1 < len(args) {
		var name string
		switch ta := args[0].(type) {
		case slip.Symbol:
			name = string(ta)
		case slip.String:
			name = string(ta)
		default:
			slip.PanicType("package", ta, "string", "symbol")
		}
		if pkg = slip.FindPackage(name); pkg == nil {
			panic(fmt.Sprintf("Package %s not found.", name))
		}
	}
	var lines []string
	if pkg != nil {
		for k, vv := range pkg.Vars {
			if !strings.Contains(k, pat) {
				continue
			}
			var line []byte
			p := vv.Pkg
			if p != &slip.CLPkg {
				line = slip.Append(line, slip.Symbol(p.Name))
				line = append(line, "::"...)
			}
			line = slip.Append(line, slip.Symbol(k))
			line = append(line, " = "...)
			line = slip.ObjectAppend(line, vv.Value())
			lines = append(lines, string(line))
		}
		// pkg.Lambdas map[string]*Lambda
		// pkg.Funcs   map[string]*FuncInfo
	} else {
		// all packages
		// only form line if owned by package
	}
	sort.Strings(lines)
	for _, line := range lines {
		// TBD print with standard-output
		fmt.Println(line)
	}
	return nil
}
