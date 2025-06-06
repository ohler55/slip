// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

import (
	"github.com/ohler55/slip"
)

// Array represents an array.
type Array struct {
	List
	prefix string
}

func arrayFromList(prefix string, list slip.List, p *slip.Printer) Node {
	array := Array{
		List: List{
			children: make([]Node, len(list)),
		},
		prefix: prefix,
	}
	for i, v := range list {
		array.children[i] = buildQNode(v, p)
	}
	return &array
}

func (array *Array) layout(left int) int {
	array.x = left
	x := left + len(array.prefix)
	if len(array.children) == 0 {
		x++
	}
	for _, n := range array.children {
		x++
		x += n.layout(x)
	}
	array.wide = x - left + 1

	return array.wide
}

func (array *Array) reorg(edge int) (w int) {
	if edge < array.right() {
		last := len(array.children) - 1
		x := len(array.prefix) // #2A( minus 1 to make all member +1
		for i, n := range array.children {
			cw := n.width()
			if last == i {
				cw++
			}
			if edge < array.x+x+cw+1 {
				n.setNewline(true)
				n.setLeft(array.x + len(array.prefix) + 1)
				x = len(array.prefix) + 1
			} else {
				x++
				n.setLeft(array.x + x)
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
		array.wide = w
	}
	return
}

func (array *Array) adjoin(b []byte) []byte {
	b = append(b, array.prefix...)
	b = append(b, '(')
	for i, n := range array.children {
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
