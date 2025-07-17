// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

// Vertical represents a list that either fully fits on one line or has each
// element on it's own line.
type Vertical struct {
	List
}

func (vert *Vertical) reorg(edge int) (w int) {
	if edge < vert.right() {
		last := len(vert.children) - 1
		for i, n := range vert.children {
			n.setLeft(vert.x + 1)
			if 0 < i {
				n.setNewline(true)
			}
			cw := n.reorg(edge)
			if last == i {
				cw++
			}
			if w < cw+1 {
				w = cw + 1
			}
		}
		vert.wide = w
	}
	return vert.wide
}
