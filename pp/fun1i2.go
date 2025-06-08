// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

import "github.com/ohler55/slip"

// Fun1i2 represents a named function with a special first argument.
type Fun1i2 struct {
	List
	name string
}

func newFun1i2(name string, args slip.List, p *slip.Printer) Node {
	fun := Fun1i2{
		List: List{children: make([]Node, len(args))},
		name: name,
	}
	for i, v := range args {
		fun.children[i] = buildNode(v, p)
	}
	return &fun
}

func (fun *Fun1i2) layout(left int) (w int) {
	fun.x = left
	cw := fun.children[0].layout(left + 2 + len(fun.name))
	fun.wide = cw + 2 + len(fun.name)
	offset := left + 2
	for _, n := range fun.children[1:] {
		if cw = n.layout(offset); fun.wide < cw+2 {
			n.setNewline(true)
			fun.wide = cw + 2
		}
	}
	return fun.wide
}

func (fun *Fun1i2) reorg(edge int) int {
	if edge < fun.right() {
		w := len(fun.name) + 1
		if edge < fun.children[0].right() {
			fun.children[0].setNewline(true)
			fun.children[0].setLeft(fun.x + 4)
			cw := fun.children[0].reorg(edge)
			if w < cw+4 {
				w = cw + 4
			}
		}
		last := len(fun.children) - 2
		for i, n := range fun.children[1:] {
			cw := n.reorg(edge)
			if last == i {
				cw++
			}
			if w < cw+2 {
				w = cw + 2
			}
		}
		fun.wide = w
	}
	return fun.wide
}

func (fun *Fun1i2) adjoin(b []byte) []byte {
	b = append(b, '(')
	b = append(b, fun.name...)
	offset := fun.x + 2
	if fun.children[0].newline() {
		b = append(b, indent[:fun.children[0].left()+1]...)
	} else {
		b = append(b, ' ')
	}
	b = fun.children[0].adjoin(b)
	for _, n := range fun.children[1:] {
		b = append(b, indent[:offset+1]...)
		b = n.adjoin(b)
	}
	return append(b, ')')
}
