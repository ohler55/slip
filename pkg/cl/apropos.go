// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
	"io"
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
			Return: "",
			Text: `Return all symbols that contain the provided _string_.
The matches are printed to _*standard-output*_ along with the package they are from and what the
symbol is associated with.`,
			Examples: []string{
				`(apropos "terpri") => nil ;; terpri (built-in) is written`,
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
			lines = append(lines, f.formVarLine(k, vv))
		}
		for k, fi := range pkg.Funcs {
			if !strings.Contains(k, pat) {
				continue
			}
			lines = append(lines, f.formFuncLine(k, fi))
		}
	} else {
		for _, pn := range slip.PackageNames() {
			pkg := slip.FindPackage(string(pn.(slip.String)))
			for k, vv := range pkg.Vars {
				if vv.Pkg != pkg || !strings.Contains(k, pat) {
					continue
				}
				lines = append(lines, f.formVarLine(k, vv))
			}
			for k, fi := range pkg.Funcs {
				if fi.Pkg != pkg || !strings.Contains(k, pat) {
					continue
				}
				lines = append(lines, f.formFuncLine(k, fi))
			}
		}
	}
	sort.Strings(lines)
	var w io.Writer = slip.StandardOutput.(io.Writer)
	for _, line := range lines {
		if _, err := w.Write(append([]byte(line), '\n')); err != nil {
			panic(err)
		}
	}
	return slip.Novalue
}

func (f *Apropos) formVarLine(k string, vv *slip.VarVal) string {
	var line []byte
	p := vv.Pkg
	if p != &slip.CLPkg {
		line = slip.Append(line, slip.Symbol(p.Name))
		line = append(line, "::"...)
	}
	line = slip.Append(line, slip.Symbol(k))
	line = append(line, " = "...)
	line = slip.ObjectAppend(line, vv.Value())

	return string(line)
}

func (f *Apropos) formFuncLine(k string, fi *slip.FuncInfo) string {
	var line []byte
	p := fi.Pkg
	if p != &slip.CLPkg {
		line = slip.Append(line, slip.Symbol(p.Name))
		line = append(line, "::"...)
	}
	line = slip.Append(line, slip.Symbol(k))
	if fi.BuiltIn {
		line = append(line, " (built-in)"...)
	} else {
		line = append(line, " (lambda)"...)
	}
	return string(line)
}
