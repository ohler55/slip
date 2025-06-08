// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

import "github.com/ohler55/slip"

// Defvar represents a defvar block.
type Defvar struct {
	List    // value and doc if present
	varName string
}

func defvarFromList(args slip.List, p *slip.Printer) Node {
	var defvar Defvar
	if sym, ok := args[0].(slip.Symbol); ok {
		defvar.varName = string(sym)
	}
	if 1 < len(args) {
		defvar.children = append(defvar.children, buildNode(args[1], p))
		if 2 < len(args) {
			if doc, ok := args[2].(slip.String); ok {
				defvar.children = append(defvar.children, &Doc{text: string(doc)})
			}
		}
	}
	return &defvar
}

func (defvar *Defvar) layout(left int) (w int) {
	defvar.x = left
	w = len(defvar.varName) + 9
	for _, n := range defvar.children {
		cw := n.layout(w)
		w += cw
	}
	w++
	defvar.wide = w

	return
}

func (defvar *Defvar) reorg(edge int) int {
	if edge < defvar.right() {
		last := len(defvar.children) - 1
		w := len(defvar.varName) + 9
		x := w
		for i, n := range defvar.children {
			if edge < x+n.width() {
				n.setNewline(true)
				x = 2
			}
			n.setLeft(x)
			cw := n.reorg(edge)
			if last == i {
				cw++
			}
			if w < n.left()+cw {
				w = n.left() + cw
			}
		}
		defvar.wide = w
	}
	return defvar.wide
}

func (defvar *Defvar) adjoin(b []byte) []byte {
	b = append(b, "(defvar "...)
	b = append(b, defvar.varName...)
	for _, n := range defvar.children {
		if n.newline() {
			b = append(b, indent[:n.left()+1]...)
		} else {
			b = append(b, ' ')
		}
		b = n.adjoin(b)
	}
	return append(b, ')')
}
