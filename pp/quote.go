// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

import "github.com/ohler55/slip"

// Quote represents a list.
type Quote struct {
	child Node
	wide  int
	x     int
}

func newQuote(obj slip.Object, p *slip.Printer) Node {
	return &Quote{child: buildQNode(obj, p)}
}

func (q *Quote) layout(left int) (w int) {
	q.x = left
	w = q.child.layout(left+1) + 1
	q.wide = w

	return
}

func (q *Quote) reorg(edge int) int {
	if edge < q.right() {
		q.wide = q.child.reorg(edge) + 1
	}
	return q.wide
}

func (q *Quote) adjoin(b []byte) []byte {
	b = append(b, '\'')
	return q.child.adjoin(b)
}

func (q *Quote) left() int {
	return q.x
}

func (q *Quote) setLeft(left int) {
	q.x = left
}

func (q *Quote) width() int {
	return q.wide
}

func (q *Quote) right() int {
	return q.x + q.wide
}

func (q *Quote) newline() bool {
	return false
}

func (q *Quote) setNewline(nl bool) {
	// never a newline
}
