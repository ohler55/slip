// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

type pList struct {
	children []pNode
	wide     int
	x        int
	y        int
}

func newPlist(obj slip.List, p *slip.Printer) pNode {
	list := pList{children: make([]pNode, len(obj))}
	for i, v := range obj {
		list.children[i] = buildPnode(v, p)
	}
	return &list
}

func (list *pList) layout(left, line int) (w int) {
	list.x = left
	list.y = line
	x := left
	if len(list.children) == 0 {
		x++
	}
	for _, n := range list.children {
		x++
		x += n.layout(x, line)
	}
	list.wide = x - left + 1

	return list.wide
}

func (list *pList) adjoin(b []byte) []byte {
	b = append(b, '(')
	y := list.y
	for i, n := range list.children {
		if 0 < i {
			if y != n.line() {
				b = append(b, indent[n.left()+1:]...)
				y++
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

func (list *pList) line() int {
	return list.y
}

func (list *pList) width() int {
	return list.wide
}

func (list *pList) right() int {
	return list.x + list.wide
}
