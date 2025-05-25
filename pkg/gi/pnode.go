// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import "github.com/ohler55/slip"

type pNode interface {
	layout(left, right, tightness int) (ok bool)
	adjoin(b []byte, left, right, tightness int) []byte
	depth() int
}

func buildPnode(obj slip.Object, p *slip.Printer) (node pNode) {

	switch to := obj.(type) {
	case slip.List:
		list := make(pList, len(to))
		for i, v := range to {
			list[i] = buildPnode(v, p)
		}
		node = list
	default:
		leaf := pLeaf{
			buf: p.Append(nil, obj, 0),
		}
		leaf.width = len([]rune(string(leaf.buf)))
		node = &leaf
	}
	// TBD

	return
}

type pList []pNode

func (list pList) layout(left, right, tightness int) (ok bool) {

	// TBD

	return
}

func (list pList) adjoin(b []byte, left, right, tightness int) []byte {

	// TBD

	return b
}

func (list pList) depth() int {
	var mx int
	for _, n := range list {
		d := n.depth()
		if mx < d {
			mx = d
		}
	}
	return mx + 1
}

type pLeaf struct {
	buf   []byte
	width int // number of runes
}

func (leaf *pLeaf) layout(left, right, tightness int) (ok bool) {

	// TBD

	return
}

func (leaf *pLeaf) adjoin(b []byte, left, right, tightness int) []byte {

	// TBD

	return b
}

func (leaf *pLeaf) depth() int {
	return 0
}
