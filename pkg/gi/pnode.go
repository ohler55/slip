// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
)

const indent = "\n                                                                " +
	"                                                                " +
	"                                                                " +
	"                                                                " // 256 wide should be enough

type pNode interface {
	layout(maxWidth, tightness int) (width int)
	adjoin(b []byte, left, right, tightness int) []byte
	depth() int
	width() int
}

func buildPnode(obj slip.Object, p *slip.Printer) (node pNode) {
	switch to := obj.(type) {
	case slip.List:
		node = newPlist(to, p)
	case *cl.Let:
		node = newPlet(to, p)
	case *cl.Letx:
		node = newPlet(to, p)
	default:
		node = pLeaf(p.Append(nil, obj, 0))
	}
	// TBD

	return
}
