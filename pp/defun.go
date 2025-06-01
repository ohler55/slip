// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

import "github.com/ohler55/slip"

// Defun represents a defun block.
type Defun struct {
	List
	name  string
	fname string
	args  Node
}

func defunFromList(name string, args slip.List, p *slip.Printer) Node {
	defun := Defun{name: name}
	if sym, ok := args[0].(slip.Symbol); ok {
		defun.fname = string(sym)
	}
	if list, ok := args[1].(slip.List); ok {
		defun.args = newPlist(list, p, true)
	}
	args = args[2:]
	defun.children = make([]Node, len(args))
	for i, v := range args {
		if i == 0 {
			if doc, ok := v.(slip.String); ok {
				defun.children[i] = &Doc{text: string(doc)}
				continue
			}
		}
		defun.children[i] = buildNode(v, p)
	}
	return &defun
}

func (defun *Defun) layout(left int) (w int) {
	defun.x = left
	w = len(defun.name) + len(defun.fname) + 3
	w += defun.args.layout(w)
	last := len(defun.children) - 1
	for i, n := range defun.children {
		n.setNewline(true)
		cw := n.layout(left + 2)
		if last == i {
			cw++
		}
		if w < cw {
			w = cw
		}
	}
	defun.wide = w

	return
}

func (defun *Defun) reorg(edge int) int {
	if edge < defun.right() {
		w := defun.args.reorg(edge)
		if w2 := defun.reorgLines(edge, 2); w < w2 {
			w = w2
		}
		defun.wide = w
	}
	return defun.wide
}

func (defun *Defun) adjoin(b []byte) []byte {
	b = append(b, '(')
	b = append(b, defun.name...)
	b = append(b, ' ')
	b = append(b, defun.fname...)
	b = append(b, ' ')
	b = defun.args.adjoin(b)
	for _, n := range defun.children {
		b = append(b, indent[:n.left()+1]...)
		b = n.adjoin(b)
	}
	return append(b, ')')
}
