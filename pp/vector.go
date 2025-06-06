// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

import (
	"github.com/ohler55/slip"
)

// Vector represents a vector.
type Vector struct {
	List
}

func vectorFromVector(sv *slip.Vector, p *slip.Printer) Node {
	list := sv.AsList()
	vector := Vector{
		List: List{
			children: make([]Node, len(list)),
		},
	}
	for i, v := range list {
		vector.children[i] = buildQNode(v, p)
	}
	return &vector
}

func (vector *Vector) layout(left int) int {
	vector.x = left
	x := left + 1
	if len(vector.children) == 0 {
		x++
	}
	for _, n := range vector.children {
		x++
		x += n.layout(x)
	}
	vector.wide = x - left + 1

	return vector.wide
}

func (vector *Vector) reorg(edge int) (w int) {
	if edge < vector.right() {
		last := len(vector.children) - 1
		x := 1 // #( minus 1 to make all member +1
		for i, n := range vector.children {
			cw := n.width()
			if last == i {
				cw++
			}
			if edge < vector.x+x+cw+1 {
				n.setNewline(true)
				n.setLeft(vector.x + 2)
				x = 2
			} else {
				x++
				n.setLeft(vector.x + x)
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
		vector.wide = w
	}
	return
}

func (vector *Vector) adjoin(b []byte) []byte {
	b = append(b, '#', '(')
	for i, n := range vector.children {
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
