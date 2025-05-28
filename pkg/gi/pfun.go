// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

type pFun struct {
	pList
	name string
}

func newPfun(name string, args slip.List, p *slip.Printer) pNode {
	fun := pFun{
		pList: pList{children: make([]pNode, len(args))},
		name:  name,
	}
	for i, v := range args {
		fun.children[i] = buildPnode(v, p)
	}
	return &fun
}

func (fun *pFun) layout(maxWidth, tightness int) (width int) {
	if fun.mod != pNotSet {
		return fun.wide
	}
	mw := maxWidth - 1
	// A function call always starts with an open paren and the function name.
	width = len(fun.name) + 1
	switch {
	case tightness == 0:
		for _, n := range fun.children {
			width++
			width += n.layout(maxWidth-width, 0)
		}
		width++
		if width <= maxWidth {
			fun.wide = width
			fun.mod = pUsual
		}
	case 0 < tightness: // pTight
		// (quux one
		//       two)
		//
		// Indicating the name len plus a paren and space added to the longest
		// child is the width.
		ct := tightness - 1
		shift := len(fun.name) + 2
		width = shift
		for i, n := range fun.children {
			cw := n.layout(mw-shift, ct)
			if i == len(fun.children)-1 { // add closing parens
				cw++
			}
			if width < cw+shift {
				width = cw + shift
			}
		}
		if width <= maxWidth {
			fun.wide = width
			fun.mod = pTight
		}
	default: // squeeze
		// (quux
		//  one
		//  two)
		ct := tightness + 1
		shift := 1
		width = len(fun.name) + 1
		for i, n := range fun.children {
			cw := n.layout(mw, ct)
			if i == len(fun.children)-1 {
				cw++
			}
			if width < cw+shift {
				width = cw + shift
			}
		}
		if width <= maxWidth {
			fun.wide = width
			fun.mod = pSqueeze
		}
	}
	return
}

func (fun *pFun) adjoin(b []byte, left, right int) []byte {
	// fmt.Printf("*** adjoin %s: [%d %d] %s width: %d\n", fun.name, left, right, fun.mod, fun.wide)
	b = append(b, '(')
	b = append(b, fun.name...)
	switch fun.mod {
	case pUsual:
		for _, n := range fun.children {
			b = append(b, ' ')
			b = n.adjoin(b, 0, right)
		}
	case pTight:
		shift := left + 2 + len(fun.name)
		w := shift
		for i, n := range fun.children {
			cw := n.width()
			if i == len(fun.children)-1 {
				cw++
			}
			switch {
			case i == 0: // first one always (quux one
				b = append(b, ' ')
				b = n.adjoin(b, shift, right)
				w = shift + cw
			case right <= w+cw+1:
				b = append(b, indent[:shift+1]...)
				b = n.adjoin(b, shift, right)
				w = shift + cw
			default:
				b = append(b, ' ')
				b = n.adjoin(b, w+1, right)
				w += cw + 1
			}
		}
	case pSqueeze:
		for _, n := range fun.children {
			b = append(b, indent[:left+2]...)
			b = n.adjoin(b, left+1, right)
		}
	}
	return append(b, ')')
}
