// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

import "github.com/ohler55/slip"

// Pair represents a key value pair.
type Pair struct {
	List
}

func newPair(left, right slip.Object, p *slip.Printer) Node {
	return &Pair{List: List{children: []Node{buildNode(left, p), buildNode(right, p)}}}
}

func (pair *Pair) layout(left int) int {
	pair.x = left
	pair.wide = 0
	for i, n := range pair.children {
		if 0 < i {
			pair.wide++
		}
		pair.wide += n.layout(left + pair.wide)
	}
	return pair.wide
}

func (pair *Pair) reorg(edge int) (w int) {
	if edge < pair.right() {
		var x int
		for i, n := range pair.children {
			cw := n.width()
			if 0 < i {
				cw++
			}
			if edge < pair.x+x+cw {
				n.setNewline(true)
				n.setLeft(pair.x)
				x = 0
			} else {
				x++
				n.setLeft(pair.x + x)
			}
			cw = n.reorg(edge)
			x += cw
			if w < x {
				w = x
			}
		}
		pair.wide = w
	}
	return pair.wide
}

func (pair *Pair) adjoin(b []byte) []byte {
	for i, n := range pair.children {
		if 0 < i {
			if n.newline() {
				b = append(b, indent[:n.left()+1]...)
			} else {
				b = append(b, ' ')
			}
		}
		b = n.adjoin(b)
	}
	return b
}
