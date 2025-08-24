// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

import (
	"github.com/ohler55/slip"
)

// DefGenMethod represents a defmethod block.
type DefGenMethod struct {
	List // forms
	name *Leaf
	qual *Leaf
	sll  Node
	doc  *Doc
}

func defGenMethod(args slip.List, p *slip.Printer) Node {
	var dm DefGenMethod
	// expect name [qualifier] specifiers [doc] forms*
	sym, _ := args[0].(slip.Symbol)
	dm.name = &Leaf{text: []byte(sym)}
	args = args[1:]

	if sym, _ = args[0].(slip.Symbol); 0 < len(sym) {
		dm.qual = &Leaf{text: []byte(sym)}
		args = args[1:]
	}
	dm.sll = argsFromList(args[0], p)
	args = args[1:]
	if ss, ok := args[0].(slip.String); ok {
		dm.doc = &Doc{text: string(ss), nl: true}
		args = args[1:]
	}
	for _, v := range args {
		dm.children = append(dm.children, buildNode(v, p))
	}
	return &dm
}

func (dm *DefGenMethod) layout(left int) (w int) {
	dm.x = left
	w = 11
	w += dm.name.layout(w)
	w++
	if dm.qual != nil {
		w += dm.qual.layout(w)
		w++
	}
	w += dm.sll.layout(w)
	if dm.doc != nil {
		_ = dm.doc.layout(left + 2)
	}
	last := len(dm.children) - 1
	for i, n := range dm.children {
		n.setNewline(true)
		cw := n.layout(left + 2)
		if last == i {
			cw++
		}
		if w < cw {
			w = cw
		}
	}
	dm.wide = w

	return
}

func (dm *DefGenMethod) reorg(edge int) int {
	if edge < dm.right() {
		w := 11
		if edge < dm.name.right() {
			dm.name.setNewline(true)
			dm.name.x = dm.x + 4
			dm.sll.setNewline(true)
			dm.sll.setLeft(dm.x + 4)
		} else if edge < dm.sll.right() {
			dm.sll.setNewline(true)
			dm.sll.setLeft(dm.x + 4)
		}
		if dm.qual != nil && edge < dm.qual.right() {
			dm.qual.setNewline(true)
			dm.qual.setLeft(dm.x + 4)
		}
		_ = dm.sll.reorg(edge)
		r := dm.sll.right()
		if w < r-dm.x {
			w = r - dm.x
		}
		if dm.doc != nil {
			w = dm.doc.reorg(edge)
		}
		last := len(dm.children) - 1
		for i, n := range dm.children {
			_ = n.reorg(edge)
			r = n.right()
			if i == last {
				r++
			}
			if w < r-dm.x {
				w = r - dm.x
			}
		}
		dm.wide = w
	}
	return dm.wide
}

func (dm *DefGenMethod) adjoin(b []byte) []byte {
	b = append(b, "(defmethod"...)
	if dm.name.newline() {
		b = append(b, indent[:dm.name.left()+1]...)
	} else {
		b = append(b, ' ')
	}
	b = dm.name.adjoin(b)
	if dm.qual != nil {
		if dm.qual.newline() {
			b = append(b, indent[:dm.qual.left()+1]...)
		} else {
			b = append(b, ' ')
		}
		b = dm.qual.adjoin(b)
	}
	if dm.sll.newline() {
		b = append(b, indent[:dm.sll.left()+1]...)
	} else {
		b = append(b, ' ')
	}
	b = dm.sll.adjoin(b)
	if dm.doc != nil {
		b = append(b, indent[:dm.doc.left()+1]...)
		b = dm.doc.adjoin(b)
	}
	for _, n := range dm.children {
		b = append(b, indent[:n.left()+1]...)
		b = n.adjoin(b)
	}
	return append(b, ')')
}
