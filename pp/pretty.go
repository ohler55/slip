// Copyright (c) 2025, Peter Ohler, All rights reserved.

// Package pp implements a LISP pretty printer.
package pp

import (
	"strings"

	"github.com/ohler55/slip"
)

const (
	indent = "\n                                                                " +
		"                                                                " +
		"                                                                " +
		"                                                                " // 256 wide should be enough
)

// PrettyAppend appends a pretty formatted object using the default printer
// setting with print variables overridden by scoped variables.
func PrettyAppend(b []byte, s *slip.Scope, obj slip.Object) []byte {
	p := *slip.DefaultPrinter()
	p.ScopedUpdate(s)

	if sym, ok := obj.(slip.Symbol); ok {
		obj = resolveSymbol(sym, s)
	}
	tree := buildNode(obj, &p)
	_ = tree.layout(0)
	_ = tree.reorg(int(p.RightMargin))
	b = tree.adjoin(b)

	return append(b, '\n')
}

func resolveSymbol(sym slip.Symbol, s *slip.Scope) slip.Object {
	if fi := slip.FindFunc(string(sym)); fi != nil {
		return fi
	}
	if s.Has(sym) {
		return s.Get(sym)
	}
	// TBD check for flavor (should it be a describe or a **defflavor**?)
	// TBD instance
	// TBD check for flavor.method or flavor:method
	// TBD check package:symbol or package::symbol
	return sym
}

func buildNode(obj slip.Object, p *slip.Printer) (node Node) {
	// Quoted values are treated as the value quoted. Lists are converted to
	// functions if possible.
top:
	switch to := obj.(type) {
	case slip.List:
		if len(to) == 0 {
			obj = nil
			goto top
		}
		if sym, ok := to[0].(slip.Symbol); ok {
			node = buildCall(sym, to[1:], p)
		} else {
			node = newPlist(to, p, false)
		}
	case *slip.Lambda:
		// buf = f.disassembleLambda(s, ta, right, ansi)
		node = &Leaf{text: p.Append(nil, slip.String(obj.String()), 0)}
		// TBD
	case *slip.FuncInfo:
		node = buildFuncInfo(to, p)
	case slip.Funky:
		node = buildCall(slip.Symbol(to.GetName()), to.GetArgs(), p)
	default:
		node = &Leaf{text: p.Append(nil, obj, 0)}
	}
	return
}

func buildQNode(obj slip.Object, p *slip.Printer) Node {
	if list, ok := obj.(slip.List); ok {
		if 0 < len(list) {
			return newPlist(list, p, true)
		}
		obj = nil
	}
	return &Leaf{text: p.Append(nil, obj, 0)}
}

func buildCall(sym slip.Symbol, args slip.List, p *slip.Printer) (node Node) {
	name := strings.ToLower(string(sym))
	switch name {
	case "quote":
		// TBD use pQuote with the tick when printing
		if 0 < len(args) {
			node = buildQNode(args[0], p)
		}
	case "let", "let*":
		node = newPlet(name, args, p)
	case "defun", "defmacro":
		node = defunFromList(name, args, p)
	default:
		node = newPfun(name, args, p)
	}
	return
}

func buildFuncInfo(fi *slip.FuncInfo, p *slip.Printer) Node {
	var defun Defun

	if fi.Kind == slip.Symbol("macro") {
		defun.name = "defmacro"
	} else {
		defun.name = "defun"
	}
	defun.fname = fi.Name
	args := &List{children: make([]Node, len(fi.Doc.Args))}
	for i, da := range fi.Doc.Args {
		if da.Default == nil {
			args.children[i] = &Leaf{text: []byte(da.Name)}
		} else {
			args.children[i] = &List{
				children: []Node{
					&Leaf{text: []byte(da.Name)},
					buildNode(da.Default, p),
				}}
		}
	}
	defun.args = args
	if 0 < len(fi.Doc.Text) {
		defun.children = append(defun.children, &Doc{text: fi.Doc.Text})
	}
	fun := fi.Create(nil).(slip.Funky)
	if lam, ok := fun.Caller().(*slip.Lambda); ok {
		for _, form := range lam.Forms {
			defun.children = append(defun.children, buildNode(form, p))
		}
	} else {
		defun.children = append(defun.children, &Leaf{text: []byte("...")})
	}
	return &defun
}
