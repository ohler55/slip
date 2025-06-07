// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

import (
	"github.com/ohler55/slip"
)

// List represents a list.
type List struct {
	children []Node
	wide     int
	x        int
	nl       bool
}

func newList(obj slip.List, p *slip.Printer, quoted bool) Node {
	list := List{children: make([]Node, len(obj))}
	if quoted {
		for i, v := range obj {
			list.children[i] = buildQNode(v, p)
		}
	} else {
		for i, v := range obj {
			list.children[i] = buildNode(v, p)
		}
	}
	return &list
}

func (list *List) layout(left int) (w int) {
	list.x = left
	x := left
	for _, n := range list.children {
		x++
		x += n.layout(x)
	}
	list.wide = x - left + 1

	return list.wide
}

func (list *List) reorg(edge int) (w int) {
	if edge < list.right() {
		last := len(list.children) - 1
		var x int
		for i, n := range list.children {
			cw := n.width()
			if last == i {
				cw++
			}
			if edge < list.x+x+cw+1 {
				n.setNewline(true)
				n.setLeft(list.x + 1)
				x = 1
			} else {
				x++
				n.setLeft(list.x + x)
			}
			cw = n.reorg(edge)
			if last == i {
				cw++
			}
			x += cw
			if w < x {
				w = x
			}
		}
		list.wide = w
	}
	return list.wide
}

func (list *List) reorgLines(edge, minWidth int) int {
	if edge < list.right() {
		w := minWidth
		last := len(list.children) - 1
		for i, n := range list.children {
			cw := n.reorg(edge)
			if last == i {
				cw++
			}
			if w < cw {
				w = cw
			}
		}
		list.wide = w
	}
	return list.wide
}

func (list *List) adjoin(b []byte) []byte {
	b = append(b, '(')
	for i, n := range list.children {
		if 0 < i {
			if n.newline() {
				b = append(b, indent[:n.left()+1]...)
			} else {
				b = append(b, ' ')
			}
		}
		b = n.adjoin(b)
	}
	return append(b, ')')
}

func (list *List) left() int {
	return list.x
}

func (list *List) setLeft(left int) {
	shift := left - list.x
	list.x = left
	for _, n := range list.children {
		n.setLeft(n.left() + shift)
	}
}

func (list *List) width() int {
	return list.wide
}

func (list *List) right() int {
	return list.x + list.wide
}

func (list *List) newline() bool {
	return list.nl
}

func (list *List) setNewline(nl bool) {
	list.nl = nl
}
