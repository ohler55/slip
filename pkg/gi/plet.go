// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import "github.com/ohler55/slip"

type pLet struct {
	pList
	name string
}

func newPlet(name string, args slip.List, p *slip.Printer) pNode {
	let := pLet{
		pList: pList{children: make([]pNode, len(args))},
		name:  name,
	}
	bindings := args[0].(slip.List)
	let.children[0] = newPbindings(bindings, p)
	for i, v := range args[1:] {
		let.children[i+1] = buildPnode(v, p)
	}
	return &let
}

func (let *pLet) layout(maxWidth, tightness int) (width int) {
	// A let always starts with '(let ' or '(let* ' for let*. The forms are
	// always indented by 2 so the layout at this level is not effected by the
	// tightness other than shift the tighness for the children if needed.
	ct := tightness
	if 0 < tightness {
		ct--
	} else if tightness < 0 {
		ct++
	}
	width = len(let.name) + 2
	width += let.children[0].layout(maxWidth-width, ct)
	var last int
	for _, n := range let.children[1:] {
		w := n.layout(maxWidth-2, ct) + 2
		last = w
		if width < w {
			width = w
		}
	}
	if last == width {
		width++
	}
	if width <= maxWidth {
		let.wide = width
		let.mod = pUsual
	}
	return
}

func (let *pLet) adjoin(b []byte, left, right int) []byte {
	b = append(b, '(')
	b = append(b, let.name...)
	b = append(b, ' ')
	// if 0 < tightness {
	// 	ct--
	// } else if tightness < 0 {
	// 	ct++
	// }
	// b = let.children[0].adjoin(b, left+len(let.name)+3, right, ct)
	// for _, n := range let.children[1:] {
	// 	b = append(b, indent[:3]...)
	// 	b = n.adjoin(b, left+2, right, ct)
	// }
	return append(b, ')')
}
