// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

type pBindings struct {
	pList
}

func newPbindings(obj List, p *Printer) pNode {
	pb := pBindings{pList: pList{children: make([]pNode, len(obj))}}
	for i, v := range obj {
		if list, ok := v.(List); ok {
			pb.children[i] = newPlist(list, p, false)
		} else {
			pb.children[i] = &pLeaf{text: p.Append(nil, obj, 0)}
		}
	}
	return &pb
}

func (pb *pBindings) layout(left int) (w int) {
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

func (pb *pBindings) reorg(edge int) int {
	return pb.reorgLines(edge, 2)
}
