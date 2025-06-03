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
	if c := slip.FindClass(string(sym)); c != nil {
		return c
	}
	if strings.ContainsRune(string(sym), ':') {
		parts := strings.Split(string(sym), ":")
		name := parts[len(parts)-1]
		if p := slip.FindPackage(parts[0]); p != nil {
			if fi := p.GetFunc(name); fi != nil {
				return fi
			}
			if obj, has := p.Get(name); has {
				return obj
			}
		} else if c := slip.FindClass(name); c != nil {
			// if f, ok := c.(*flavors.Flavor); ok {
			// 	// ma := f.GetMethod(name)
			// 	return f // TBD maybe build a set of defmethods?
			// }
			return c
		}
	}
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
		node = buildLambda(to, p)
	case *slip.FuncInfo:
		node = buildFuncInfo(to, p)
	case slip.Funky:
		node = buildCall(slip.Symbol(to.GetName()), to.GetArgs(), p)
	// case *flavors.Flavor:
	// 	// TBD this creates an import loop, maybe add to class interface to have func that returns def as list
	// 	node = &Leaf{text: p.Append(nil, obj, 0)}
	// case *flavors.Instance:
	// 	// TBD
	// 	node = &Leaf{text: p.Append(nil, obj, 0)}
	// case *[]flavors.Method:
	// 	// TBD
	// 	node = &Leaf{text: p.Append(nil, obj, 0)}
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
		if 0 < len(args) {
			node = newQuote(args[0], p)
		}
	case "let", "let*":
		node = newLet(name, args, p)
	case "lambda":
		node = lambdaFromList(args, p)
	case "defun", "defmacro":
		node = defunFromList(name, args, p)
	case "defvar", "defconstant", "defparameter", "defpackage":
		node = defvarFromList(args, p)
	case "cond":
		node = newFun(name, args, p, 2)
	case "block",
		"dotimes",
		"dolist",
		"do",
		"do*",
		"do-all-symbols",
		"do-external-symbols",
		"do-symbols",
		"dovector",
		"with-input-from-octets",
		"with-zip-reader",
		"with-zip-writer",
		"with-input-from-string",
		"with-open-file",
		"with-open-stream",
		"with-output-to-string",
		"with-standard-io-syntax":
		node = newFun1i2(name, args, p)
	case "make-instance":
		node = newFun1i2(name, args, p)
	case "defflavor":
		// TBD
		node = newFun(name, args, p, 1)
	case "defmethod":
		// TBD
		node = newFun(name, args, p, 1)
	default:
		node = newFun(name, args, p, 1)
	}
	return
}

func buildFuncInfo(fi *slip.FuncInfo, p *slip.Printer) Node {
	defun := Defun{
		fname: fi.Name,
		args:  buildDocArgs(fi.Doc.Args, p),
	}
	if fi.Kind == slip.Symbol("macro") {
		defun.name = "defmacro"
	} else {
		defun.name = "defun"
	}
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

func buildLambda(lam *slip.Lambda, p *slip.Printer) Node {
	ln := Lambda{
		args: buildDocArgs(lam.Doc.Args, p),
		List: List{
			children: make([]Node, len(lam.Forms)),
		},
	}
	for i, form := range lam.Forms {
		ln.children[i] = buildNode(form, p)
	}
	return &ln
}

func buildDocArgs(docArgs []*slip.DocArg, p *slip.Printer) Node {
	args := &List{children: make([]Node, len(docArgs))}
	for i, da := range docArgs {
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
	return args
}
