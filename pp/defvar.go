// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

import (
	"github.com/ohler55/slip"
)

// Defvar represents a defvar block.
type Defvar struct {
	List    // value and doc if present
	name    string
	varName string
}

func defvarFromList(name string, args slip.List, p *slip.Printer) Node {
	defvar := Defvar{name: name}
	if sym, ok := args[0].(slip.Symbol); ok {
		defvar.varName = string(sym)
	}
	if 1 < len(args) {
		defvar.children = append(defvar.children, buildNode(args[1], p))
		if 2 < len(args) {
			if doc, ok := args[2].(slip.String); ok {
				defvar.children = append(defvar.children, &Doc{text: string(doc), nl: true})
			}
		}
	}
	return &defvar
}

func (defvar *Defvar) layout(left int) (w int) {
	defvar.x = left
	w = len(defvar.name) + len(defvar.varName) + 3
	if 0 < len(defvar.children) {
		w += defvar.children[0].layout(w)
		if 1 < len(defvar.children) {
			// docs are always on a new line
			cw := defvar.children[1].layout(2)
			if w < cw+2 {
				w = cw + 2
			}
		}
	}
	w++
	defvar.wide = w

	return
}

func (defvar *Defvar) reorg(edge int) int {
	if edge < defvar.right() && 1 < len(defvar.children) {
		defvar.wide = len(defvar.name) + len(defvar.varName) + 3
		defvar.children[1].setLeft(2)
		cw := defvar.children[1].reorg(edge) + 1
		if defvar.wide < cw+2 {
			defvar.wide = cw + 2
		}
	}
	return defvar.wide
}

func (defvar *Defvar) adjoin(b []byte) []byte {
	b = append(b, '(')
	b = append(b, defvar.name...)
	b = append(b, ' ')
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
