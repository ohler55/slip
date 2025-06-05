// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

import (
	"github.com/ohler55/slip"
)

// Defmethod represents a defmethod block.
type Defmethod struct {
	List
	name string
}

func defmethodFromList(name string, args slip.List, p *slip.Printer) Node {
	dm := Defmethod{
		List: List{
			children: make([]Node, len(args)),
		},
		name: name,
	}
	meth, ok := args[0].(slip.List)
	if !ok {
		meth = slip.List{args[0]}
	}
	dm.children[0] = newList(meth, p, true)
	dm.children[1] = argsFromList(args[1], p)
	for i, v := range args[2:] {
		dm.children[i+2] = buildNode(v, p)
	}
	return &dm
}

func (dm *Defmethod) layout(left int) (w int) {
	dm.x = left
	w = 2 + len(dm.name)
	w += dm.children[0].layout(w) // flavor, daemon, and method
	w++
	w += dm.children[1].layout(w) // arguments
	rest := dm.children[2:]
	last := len(rest) - 1
	for i, n := range rest {
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

func (dm *Defmethod) reorg(edge int) int {
	if edge < dm.right() {
		w := dm.children[0].reorg(edge) + 2 + len(dm.name) // (defmethod + space + length of method spec
		if edge < dm.children[1].right() {                 // args list
			dm.children[1].setNewline(true)
			dm.children[1].setLeft(dm.x + 4)
		}
		last := len(dm.children) - 1
		for i, n := range dm.children {
			_ = n.reorg(edge)
			r := n.right()
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

func (dm *Defmethod) adjoin(b []byte) []byte {
	b = append(b, '(')
	b = append(b, dm.name...)
	for _, n := range dm.children {
		if n.newline() {
			b = append(b, indent[:n.left()+1]...)
		} else {
			b = append(b, ' ')
		}
		b = n.adjoin(b)
	}
	return append(b, ')')
}
