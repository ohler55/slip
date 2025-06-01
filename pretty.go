// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import (
	"strings"
)

const (
	indent = "\n                                                                " +
		"                                                                " +
		"                                                                " +
		"                                                                " // 256 wide should be enough
)

type pNode interface {
	layout(left int) (w int)
	reorg(edge int) (w int)
	adjoin(b []byte) []byte
	width() int
	left() int
	setLeft(left int)
	right() int
	newline() bool
	setNewline(nl bool)
}

// PrettyAppend appends a pretty formatted object using the default printer
// setting with print variables overridden by scoped variables.
func PrettyAppend(b []byte, s *Scope, obj Object) []byte {
	p := *DefaultPrinter()
	p.ScopedUpdate(s)

	if sym, ok := obj.(Symbol); ok {
		obj = resolveSymbol(sym, s)
	}
	tree := buildPnode(obj, &p)
	_ = tree.layout(0)
	_ = tree.reorg(int(p.RightMargin))
	b = tree.adjoin(b)

	return append(b, '\n')
}

func resolveSymbol(sym Symbol, s *Scope) Object {
	if fi := FindFunc(string(sym)); fi != nil {
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

func buildPnode(obj Object, p *Printer) (node pNode) {
	// Quoted values are treated as the value quoted. Lists are converted to
	// functions if possible.
top:
	switch to := obj.(type) {
	case List:
		if len(to) == 0 {
			obj = nil
			goto top
		}
		if sym, ok := to[0].(Symbol); ok {
			node = buildPcall(sym, to[1:], p)
		} else {
			node = newPlist(to, p, false)
		}
	case *Lambda:
		// buf = f.disassembleLambda(s, ta, right, ansi)
		node = &pLeaf{text: p.Append(nil, String(obj.String()), 0)}
		// TBD
	case *FuncInfo:
		node = buildPfuncInfo(to, p)
	case Funky:
		node = buildPcall(Symbol(to.GetName()), to.GetArgs(), p)
	default:
		node = &pLeaf{text: p.Append(nil, obj, 0)}
	}
	return
}

func buildPQnode(obj Object, p *Printer) pNode {
	if list, ok := obj.(List); ok {
		if 0 < len(list) {
			return newPlist(list, p, true)
		}
		obj = nil
	}
	return &pLeaf{text: p.Append(nil, obj, 0)}
}

func buildPcall(sym Symbol, args List, p *Printer) (node pNode) {
	name := strings.ToLower(string(sym))
	switch name {
	case "quote":
		// TBD use pQuote with the tick when printing
		if 0 < len(args) {
			node = buildPQnode(args[0], p)
		}
	case "let", "let*":
		node = newPlet(name, args, p)
	case "defun", "defmacro":
		node = pDefunFromList(name, args, p)
	default:
		node = newPfun(name, args, p)
	}
	return
}

func buildPfuncInfo(fi *FuncInfo, p *Printer) pNode {
	var defun pDefun

	if fi.Kind == Symbol("macro") {
		defun.name = "defmacro"
	} else {
		defun.name = "defun"
	}
	defun.fname = fi.Name
	args := &pList{children: make([]pNode, len(fi.Doc.Args))}
	for i, da := range fi.Doc.Args {
		if da.Default == nil {
			args.children[i] = &pLeaf{text: []byte(da.Name)}
		} else {
			args.children[i] = &pList{
				children: []pNode{
					&pLeaf{text: []byte(da.Name)},
					buildPnode(da.Default, p),
				}}
		}
	}
	defun.args = args
	if 0 < len(fi.Doc.Text) {
		defun.children = append(defun.children, &pDoc{text: fi.Doc.Text})
	}
	fun := fi.Create(nil).(Funky)
	if lam, ok := fun.Caller().(*Lambda); ok {
		for _, form := range lam.Forms {
			defun.children = append(defun.children, buildPnode(form, p))
		}
	} else {
		defun.children = append(defun.children, &pLeaf{text: []byte("...")})
	}
	return &defun
}
