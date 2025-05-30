// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

type pList struct {
	children []pNode
	wide     int
	x        int
	nl       bool
}

func newPlist(obj slip.List, p *slip.Printer) pNode {
	list := pList{children: make([]pNode, len(obj))}
	for i, v := range obj {
		list.children[i] = buildPnode(v, p)
	}
	return &list
}

func (list *pList) reorg(edge int) int {
	if list.right() <= edge {
		return list.wide
	}
	var tight bool
	last := len(list.children) - 1
	for i, n := range list.children {
		r := n.right()
		if last == i {
			r++
		}
		if edge < r {
			tight = true
			break
		}
	}
	if tight {
		list.wide = 1
		// TBD could be smarter and allow multiple on a line
		for i, n := range list.children {
			if 0 < i {
				n.setNewline(true)
			}
			n.setLeft(list.x + 1)
			cw := n.reorg(edge)
			if last == i {
				cw++
			}
			if list.wide < cw+1 {
				list.wide = cw + 1
			}
		}
	}
	return list.wide
}

func (list *pList) layout(left int) (w int) {
	list.x = left
	x := left
	if len(list.children) == 0 {
		x++
	}
	for _, n := range list.children {
		x++
		x += n.layout(x)
	}
	list.wide = x - left + 1

	return list.wide
}

func (list *pList) adjoin(b []byte) []byte {
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

func (list *pList) left() int {
	return list.x
}

func (list *pList) setLeft(left int) {
	list.x = left
}

func (list *pList) width() int {
	return list.wide
}

func (list *pList) right() int {
	return list.x + list.wide
}

func (list *pList) newline() bool {
	return list.nl
}

func (list *pList) setNewline(nl bool) {
	list.nl = nl
}
