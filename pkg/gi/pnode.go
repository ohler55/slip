// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

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

func buildPnode(obj slip.Object, p *slip.Printer) (node pNode) {
	// TBD don't convert to funcs, leave as basics but handle funcs by converting to list
	// keep mode arg for quoted, usual, macro?

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
			node = buildPcall(sym, to[1:], p)
		} else {
			node = newPlist(to, p)
		}
	// case slip.Symbol:
	// 	// TBD lookup and build list/forms
	// 	if fi := slip.FindFunc(string(to)); fi != nil {
	// 		obj = fi.Create(nil)
	// 		goto top
	// 	}
	// case *cl.Quote:
	// 	if 0 < len(to.Args) {
	// 		obj = to.Args[0]
	// 	}
	// case *cl.Let:
	// 	// node = newPlet(to, p)
	// case *cl.Letx:
	// 	// node = newPlet(to, p)
	default:
		node = &pLeaf{text: p.Append(nil, obj, 0)}
	}
	// TBD

	return
}

func buildPcall(sym slip.Symbol, args slip.List, p *slip.Printer) (node pNode) {
	name := strings.ToLower(string(sym))
	switch name {
	case "let", "let*":
		// node = newPlet(name, args, p)
	default:
		node = newPfun(name, args, p)
	}
	return
}
