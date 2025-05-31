// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
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

	tree := buildPnode(obj, &p)
	_ = tree.layout(0)
	_ = tree.reorg(int(p.RightMargin))
	b = tree.adjoin(b)

	return append(b, '\n')
}

func buildPnode(obj Object, p *Printer) (node pNode) {
	// TBD don't convert to funcs, leave as basics but handle funcs by converting to list
	// keep mode arg for quoted, usual, macro?

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
	case Symbol:
		// TBD lookup and build list/forms or vars?
		// if fi := FindFunc(string(to)); fi != nil {
		// 	obj = fi.Create(nil)
		// 	goto top
		// }
	case Funky:
		name := to.GetName()
		args := to.GetArgs()
		fmt.Printf("*** funky %T %s\n", to, name)
		switch name {
		case "quote":
			if 0 < len(args) {
				node = buildPQnode(args[0], p)
			}
		default:
			// TBD other types
			node = &pLeaf{text: p.Append(nil, obj, 0)}
		}
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
	case "let", "let*":
		// node = newPlet(name, args, p)
	default:
		node = newPfun(name, args, p)
	}
	return
}
