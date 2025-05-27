// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import "github.com/ohler55/slip"

type pBindings struct {
	pList
}

func newPbindings(obj slip.List, p *slip.Printer) pNode {
	pb := pBindings{pList: pList{children: make([]pNode, len(obj))}}
	for i, v := range obj {
		pb.children[i] = buildPnode(v, p)
	}
	return &pb
}

func (pb *pBindings) layout(maxWidth, tightness int) (width int) {
	ct := tightness
	if 0 < tightness {
		ct--
	} else if tightness < 0 {
		ct++
	}
	width = 2
	var last int
	for _, n := range pb.children {
		w := n.layout(maxWidth-1, ct) + 2
		last = w
		if width < w {
			width = w
		}
	}
	if last == width {
		width++
	}
	if width <= maxWidth {
		pb.wide = width
	}
	return
}

func (pb *pBindings) adjoin(b []byte, left, right, tightness int) []byte {
	b = append(b, '(')
	ct := tightness
	if 0 < tightness {
		ct--
	} else if tightness < 0 {
		ct++
	}
	for i, n := range pb.children {
		if 0 < i {
			b = append(b, indent[:left+1]...)
		}
		b = n.adjoin(b, left+1, right, ct)
	}
	return append(b, ')')
}
