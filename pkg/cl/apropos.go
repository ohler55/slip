// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
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

// Call the function with the arguments provided.
func (f *Apropos) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) < 1 || 2 < len(args) {
		slip.PanicArgCount(f, 1, 2)
	}
	var pat string
	switch ta := args[0].(type) {
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
		switch ta := args[1].(type) {
		case slip.Symbol:
			name = string(ta)
		case slip.String:
			name = string(ta)
		default:
			slip.PanicType("package", ta, "string", "symbol")
		}
		if pkg = slip.FindPackage(name); pkg == nil {
			slip.NewPanic("Package %s not found.", name)
		}
	}
	pr := *slip.DefaultPrinter()
	pr.ScopedUpdate(s)
	pr.Readably = true

	var lines []string
	if pkg != nil {
		pkg.EachVarVal(func(name string, vv *slip.VarVal) {
			if strings.Contains(name, pat) {
				lines = append(lines, f.formVarLine(name, vv, &pr))
			}
		})
		pkg.EachFuncInfo(func(fi *slip.FuncInfo) {
			if strings.Contains(fi.Name, pat) {
				lines = append(lines, f.formFuncLine(fi.Name, fi))
			}
		})
	} else {
		for _, pn := range slip.PackageNames() {
			pkg := slip.FindPackage(string(pn.(slip.String)))
			pkg.EachVarVal(func(name string, vv *slip.VarVal) {
				if vv.Pkg == pkg && strings.Contains(name, pat) {
					lines = append(lines, f.formVarLine(name, vv, &pr))
				}
			})
			pkg.EachFuncInfo(func(fi *slip.FuncInfo) {
				if fi.Pkg == pkg && strings.Contains(fi.Name, pat) {
					lines = append(lines, f.formFuncLine(fi.Name, fi))
				}
			})
		}
	}
	for k, c := range slip.Constants {
		if (pkg != nil && pkg != c.Pkg) || !strings.Contains(k, pat) {
			continue
		}
		lines = append(lines, f.formConstantLine(k, c, &pr))
	}
	sort.Strings(lines)
	w := slip.StandardOutput.(io.Writer)
	for _, line := range lines {
		if _, err := w.Write(append([]byte(line), '\n')); err != nil {
			slip.PanicStream(slip.StandardOutput.(slip.Stream), "%s", err)
		}
	}
	return slip.Novalue
}

func (f *Apropos) formVarLine(k string, vv *slip.VarVal, pr *slip.Printer) string {
	var line []byte
	p := vv.Pkg
	if p != &slip.CLPkg {
		line = slip.Append(line, slip.Symbol(p.Name))
		line = append(line, ':')
	}
	line = slip.Append(line, slip.Symbol(k))
	line = append(line, " = "...)
	line = pr.Append(line, vv.Value(), 0)

	return string(line)
}

func (f *Apropos) formConstantLine(k string, c *slip.Constant, pr *slip.Printer) string {
	var line []byte
	p := c.Pkg
	if p != &slip.CLPkg {
		line = slip.Append(line, slip.Symbol(p.Name))
		line = append(line, ':')
	}
	line = slip.Append(line, slip.Symbol(k))
	line = append(line, " = "...)
	line = pr.Append(line, c.Value, 0)

	return string(line)
}

func (f *Apropos) formFuncLine(k string, fi *slip.FuncInfo) string {
	var line []byte
	p := fi.Pkg
	if p != &slip.CLPkg {
		line = slip.Append(line, slip.Symbol(p.Name))
		line = append(line, ':')
	}
	line = slip.Append(line, slip.Symbol(k))
	line = append(line, " ("...)
	line = append(line, fi.Hierarchy()[0]...)
	line = append(line, ')')

	return string(line)
}
