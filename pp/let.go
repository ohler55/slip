// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

import "github.com/ohler55/slip"

type Let struct {
	List
	name string
}

func newLet(name string, args slip.List, p *slip.Printer) Node {
	let := Let{
		List: List{children: make([]Node, len(args))},
		name: name,
	}
	bindings := args[0].(slip.List)
	let.children[0] = newPbindings(bindings, p)
	for i, v := range args[1:] {
		let.children[i+1] = buildNode(v, p)
	}
	return &let
}

func (let *Let) layout(left int) (w int) {
	let.x = left
	w = len(let.name) + 2
	last := len(let.children) - 1
	for i, n := range let.children {
		if i == 0 {
			if cw := n.layout(left + 2 + len(let.name)); w < cw {
				w = cw
			}
		} else {
			n.setNewline(true)
			cw := n.layout(left + 2)
			if last == i {
				cw++
			}
			if w < cw {
				w = cw
			}
		}
	}
	let.wide = w

	return
}

func (let *Let) reorg(edge int) int {
	return let.reorgLines(edge, len(let.name)+2)
}

func (let *Let) adjoin(b []byte) []byte {
	b = append(b, '(')
	b = append(b, let.name...)
	b = append(b, ' ')
	for i, n := range let.children {
		if 0 < i { // Binding are always on the same line as let or let*
			b = append(b, indent[:n.left()+1]...)
		}
		b = n.adjoin(b)
	}
	return append(b, ')')
}
