// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

import "github.com/ohler55/slip"

// OptPairs represents an options that starts with a symbol and is followed by
// key-value pairs.
type OptPairs struct {
	List
}

func newOptPairs(args slip.List, p *slip.Printer) Node {
	var op OptPairs
	op.children = append(op.children, buildNode(args[0], p))
	for i := 1; i < len(args); i++ {
		if i+1 < len(args) {
			op.children = append(op.children, newPair(args[i], args[i+1], p))
			i++
		} else {
			op.children = append(op.children, buildNode(args[i], p))
		}
	}
	return &op
}

func (op *OptPairs) reorg(edge int) (w int) {
	if edge < op.right() {
		last := len(op.children) - 1
		for i, n := range op.children {
			n.setLeft(op.x + 1)
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
		op.wide = w
	}
	return op.wide
}
