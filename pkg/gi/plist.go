// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"fmt"

	"github.com/ohler55/slip"
)

type pList struct {
	children []pNode
	wide     int
	mod      pMode
}

func newPlist(obj slip.List, p *slip.Printer) pNode {
	list := pList{children: make([]pNode, len(obj))}
	for i, v := range obj {
		list.children[i] = buildPnode(v, p)
	}
	return &list
}

func (list *pList) layout(maxWidth, tightness int) (width int) {
	mw := maxWidth - 1
	// A list always starts with an open paren.
	width = 1
	if tightness == 0 {
		width = 0
		for _, n := range list.children {
			width++
			width += n.layout(maxWidth-width, 0)
		}
		width++ // closing paren
		if width <= maxWidth {
			list.wide = width
			list.mod = pUsual
		}
	} else {
		ct := tightness - 1
		if tightness < 0 {
			ct = tightness + 1
		}
		shift := 1
		width = shift
		for i, n := range list.children {
			cw := n.layout(mw-shift, ct)
			if i == len(list.children)-1 { // add closing parens
				cw++
			}
			if width < cw+shift {
				width = cw + shift
			}
		}
		if width <= maxWidth {
			list.wide = width
			if 0 < tightness {
				list.mod = pTight
			} else {
				list.mod = pSqueeze
			}
		}
	}
	return
}

func (list *pList) adjoin(b []byte, left, right int) []byte {
	fmt.Printf("*** adjoin list: [%d %d] %s width: %d\n", left, right, list.mod, list.wide)
	b = append(b, '(')
	switch list.mod {
	case pUsual:
		for i, n := range list.children {
			if 0 < i {
				b = append(b, ' ')
			}
			b = n.adjoin(b, 0, right)
		}
	case pTight:
		w := 1
		for i, n := range list.children {
			cw := n.width()
			if i == len(list.children)-1 { // add closing parens
				cw++
			}
			if i == 0 { // no choice on the first one, always (quux
				b = n.adjoin(b, w, right)
				w = cw + 1
			} else if right <= w+cw+1 {
				b = append(b, indent[:left+2]...)
				b = n.adjoin(b, left+1, right)
				w = cw + 1
			} else {
				if 0 < i {
					b = append(b, ' ')
				}
				b = n.adjoin(b, left+w+1, right)
				w += cw + 1
			}
		}
	case pSqueeze:
		for i, n := range list.children {
			if 0 < i {
				b = append(b, indent[:left+2]...)
			}
			b = n.adjoin(b, left+1, right)
		}
	}
	return append(b, ')')
}

func (list *pList) depth() int {
	var mx int
	for _, n := range list.children {
		d := n.depth()
		if mx < d {
			mx = d
		}
	}
	return mx + 1
}

func (list *pList) width() int {
	return list.wide
}

func (list *pList) mode() pMode {
	return list.mod
}
