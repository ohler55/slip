// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

import "github.com/ohler55/slip"

// Bindings represents the bindings in a let or let* block.
type Bindings struct {
	List
}

func newPbindings(obj slip.List, p *slip.Printer) Node {
	pb := Bindings{List: List{children: make([]Node, len(obj))}}
	for i, v := range obj {
		if list, ok := v.(slip.List); ok {
			pb.children[i] = newPlist(list, p, false)
		} else {
			pb.children[i] = &Leaf{text: p.Append(nil, obj, 0)}
		}
	}
	return &pb
}

func (pb *Bindings) layout(left int) (w int) {
	pb.x = left
	x := left + 1
	last := len(pb.children) - 1
	for i, n := range pb.children {
		cw := n.layout(x)
		if 0 < i {
			n.setNewline(true)
		}
		if last == i {
			cw++
		}
		if w < cw {
			w = cw
		}
	}
	pb.wide = w

	return
}

func (pb *Bindings) reorg(edge int) int {
	return pb.reorgLines(edge, 2)
}
