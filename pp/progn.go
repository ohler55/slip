// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

import "github.com/ohler55/slip"

// Progn represents a named function with a special first argument.
type Progn struct {
	List
}

func newProgn(args slip.List, p *slip.Printer) Node {
	fun := Progn{
		List: List{children: make([]Node, len(args))},
	}
	for i, v := range args {
		fun.children[i] = buildNode(v, p)
		if 0 < i {
			fun.children[i].setNewline(true)
		}
	}
	return &fun
}

func (fun *Progn) layout(left int) (w int) {
	fun.x = left
	fun.wide = 6
	offset := left + 2
	for _, n := range fun.children {
		if cw := n.layout(offset); fun.wide < cw+2 {
			fun.wide = cw + 2
		}
	}
	return fun.wide
}

func (fun *Progn) reorg(edge int) int {
	if edge < fun.right() {
		w := 6
		last := len(fun.children) - 1
		for i, n := range fun.children {
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

func (fun *Progn) adjoin(b []byte) []byte {
	b = append(b, "(progn"...)
	offset := fun.x + 2
	for _, n := range fun.children {
		b = append(b, indent[:offset+1]...)
		b = n.adjoin(b)
	}
	return append(b, ')')
}
