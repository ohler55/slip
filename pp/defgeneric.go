// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

import (
	"github.com/ohler55/slip"
)

// Defgeneric represents a defmethod block.
type Defgeneric struct {
	List // options
	name *Leaf
	ll   Node
}

func defGeneric(args slip.List, p *slip.Printer) Node {
	var dg Defgeneric
	sym, _ := args[0].(slip.Symbol)
	dg.name = &Leaf{text: []byte(sym)}
	args = args[1:]
	dg.ll = argsFromList(args[0], p)
	args = args[1:]
	for _, v := range args {
		if option, ok := v.(slip.List); ok && 1 < len(option) && option[0] == slip.Symbol(":method") {
			dg.children = append(dg.children, methodOption(option[1:], p))
		} else {
			dg.children = append(dg.children, buildNode(v, p))
		}
	}
	return &dg
}

func (dg *Defgeneric) layout(left int) (w int) {
	dg.x = left
	w = 12
	w += dg.name.layout(w)
	w++
	w += dg.ll.layout(w)
	last := len(dg.children) - 1
	for i, n := range dg.children {
		n.setNewline(true)
		cw := n.layout(left + 2)
		if last == i {
			cw++
		}
		if w < cw {
			w = cw
		}
	}
	dg.wide = w

	return
}

func (dg *Defgeneric) reorg(edge int) int {
	if edge < dg.right() {
		w := 11
		if edge < dg.name.right() {
			dg.name.setNewline(true)
			dg.name.x = dg.x + 4
			dg.ll.setNewline(true)
			dg.ll.setLeft(dg.x + 4)
		} else if edge < dg.ll.right() {
			dg.ll.setNewline(true)
			dg.ll.setLeft(dg.x + 4)
		}
		_ = dg.ll.reorg(edge)
		r := dg.ll.right()
		if w < r-dg.x {
			w = r - dg.x
		}
		last := len(dg.children) - 1
		for i, n := range dg.children {
			_ = n.reorg(edge)
			r = n.right()
			if i == last {
				r++
			}
			if w < r-dg.x {
				w = r - dg.x
			}
		}
		dg.wide = w
	}
	return dg.wide
}

func (dg *Defgeneric) adjoin(b []byte) []byte {
	b = append(b, "(defgeneric"...)
	if dg.name.newline() {
		b = append(b, indent[:dg.name.left()+1]...)
	} else {
		b = append(b, ' ')
	}
	b = dg.name.adjoin(b)
	if dg.ll.newline() {
		b = append(b, indent[:dg.ll.left()+1]...)
	} else {
		b = append(b, ' ')
	}
	b = dg.ll.adjoin(b)
	for _, n := range dg.children {
		b = append(b, indent[:n.left()+1]...)
		b = n.adjoin(b)
	}
	return append(b, ')')
}
