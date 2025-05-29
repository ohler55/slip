// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import "github.com/ohler55/slip"

type pBindings struct {
	pList
}

func newPbindings(obj slip.List, p *slip.Printer) pNode {
	pb := pBindings{pList: pList{children: make([]pNode, len(obj))}}
	for i, v := range obj {
		if list, ok := v.(slip.List); ok {
			pb.children[i] = newPlist(list, p)
		} else {
			pb.children[i] = &pLeaf{text: p.Append(nil, obj, 0)}
		}
	}
	return &pb
}

func (pb *pBindings) layout(left, line int) (w int) {
	pb.x = left
	pb.y = line

	// TBD

	return 0
}

func (pb *pBindings) adjoin(b []byte) []byte {
	b = append(b, '(')

	// TBD

	return append(b, ')')
}
