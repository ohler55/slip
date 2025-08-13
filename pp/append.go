// Copyright (c) 2025, Peter Ohler, All rights reserved.

// Package pp implements a LISP pretty printer.
package pp

import (
	"fmt"
	"math"
	"strings"

	"github.com/ohler55/slip"
)

const (
	indent = "\n                                                                " +
		"                                                                " +
		"                                                                " +
		"                                                                " // 256 wide should be enough
)

// This interface is needed since importing flavors causes an undetectable
// import loop.
type hasDefMethodList interface {
	// DefMethodList returns a list that can be evaluated to define a method
	// on the class or nil if no method is defined by the class.
	DefMethodList(method, daemon string, inherited bool) slip.List
}

// Append appends a pretty formatted object using the default printer setting
// with print variables overridden by scoped variables.
func Append(b []byte, s *slip.Scope, obj slip.Object) []byte {
	p := slip.Printer{
		ANSI:        false,
		Array:       true,
		Base:        10,
		Case:        slip.Symbol(":downcase"),
		Circle:      false,
		Escape:      true,
		Gensym:      true,
		Lambda:      false,
		Length:      math.MaxInt,
		Level:       math.MaxInt,
		Lines:       math.MaxInt,
		Prec:        -1,
		MiserWidth:  0,
		Pretty:      false,
		Radix:       false,
		Readably:    true,
		RightMargin: 120,
	}
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
		}
		if c, _ := slip.FindClass(parts[0]).(hasDefMethodList); c != nil {
			daemon := "primary"
			if len(parts) == 3 && 1 < len(parts[1]) {
				daemon = parts[1]
			}
			return c.DefMethodList(":"+name, ":"+daemon, false)
		}
	}
	if fi := slip.FindFunc(string(sym)); fi != nil {
		return fi
	}
	if c := slip.FindClass(string(sym)); c != nil {
		return c
	}
	if s.Has(sym) {
		return s.Get(sym)
	}
	return sym
}

func buildNode(obj slip.Object, p *slip.Printer) (node Node) {
	// Quoted values are treated as the value quoted. Lists are converted to
	// functions if possible.
	switch to := obj.(type) {
	case nil:
		node = &Leaf{text: []byte("nil")}
	case slip.List:
		if 0 < len(to) {
			if sym, ok := to[0].(slip.Symbol); ok {
				node = buildCall(sym, to[1:], p)
				break
			}
			node = newList(to, p, false)
			break
		}
		node = &Leaf{text: []byte("nil")}
	case *slip.Lambda:
		node = buildLambda(to, p)
	case *slip.Vector:
		node = arrayFromList("#", to.AsList(), p)
	case slip.Octets:
		node = arrayFromList("#", to.AsList(), p)
	case *slip.Array:
		prefix := "#"
		switch to.Rank() {
		case 0:
			prefix = "#0A"
		case 1:
			// no change
		default:
			prefix = fmt.Sprintf("#%dA", to.Rank())
		}
		node = arrayFromList(prefix, to.AsList(), p)
	case slip.Funky:
		node = buildCall(slip.Symbol(to.GetName()), to.GetArgs(), p)
	case slip.Symbol:
		node = &Leaf{text: []byte(to)}
	default:
		form := to.LoadForm()
		if dl, _ := form.(slip.List); 0 < len(dl) {
			if sym, ok := dl[0].(slip.Symbol); ok {
				node = buildCall(sym, dl[1:], p)
			}
		}
		if node == nil {
			node = &Leaf{text: p.Append(nil, obj, 0)}
		}
	}
	return
}

func buildQNode(obj slip.Object, p *slip.Printer) Node {
	if list, ok := obj.(slip.List); ok && 0 < len(list) {
		return newList(list, p, true)
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
	case "defvar", "defconstant", "defparameter":
		node = defvarFromList(name, args, p)
	case "cond":
		node = newFun(name, args, p, 2)
	case "progn":
		node = newProgn(args, p)
	case "block",
		"defpackage",
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
		"with-standard-io-syntax",
		"make-instance":
		node = newFun1i2(name, args, p)
	case "defflavor":
		node = defflavorFromList(args, p)
	case "defmethod", "defwhopper":
		if _, ok := args[0].(slip.List); ok {
			node = defmethodFromList(name, args, p)
		} else {
			node = defGenMethod(args, p)
		}
	case "defgeneric":
		node = defGeneric(args, p)
	case "defclass", "define-condition":
		node = defclassFromList(name, args, p)
	default:
		if name[0] == ':' { // some option
			list := &List{children: []Node{&Leaf{text: []byte(name)}}}
			for _, a := range args {
				list.children = append(list.children, buildNode(a, p))
			}
			node = list
		} else {
			node = newFun(name, args, p, 1)
		}
	}
	return
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
