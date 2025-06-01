// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

import "github.com/ohler55/slip"

// Fun represents a named function.
type Fun struct {
	List
	name  string
	loose bool
}

func newPfun(name string, args slip.List, p *slip.Printer) Node {
	fun := Fun{
		List: List{children: make([]Node, len(args))},
		name: name,
	}
	for i, v := range args {
		fun.children[i] = buildNode(v, p)
	}
	return &fun
}

func (fun *Fun) layout(left int) (w int) {
	fun.x = left
	x := left + 1 + len(fun.name)
	for _, n := range fun.children {
		x++
		x += n.layout(x)
	}
	fun.wide = x - left + 1

	return fun.wide
}

func (fun *Fun) reorg(edge int) int {
	if edge < fun.right() {
		var (
			tight bool
			mw    int
		)
		last := len(fun.children) - 1
		for i, n := range fun.children {
			cw := n.width()
			r := n.right()
			if last == i {
				r++
				cw++
			}
			if mw < cw {
				mw = cw
			}
			if edge < r {
				tight = true
			}
		}
		if tight {
			fun.wide = 1
			offset := fun.x + 1
			if offset+len(fun.name)+mw < edge {
				fun.loose = true
				offset = fun.x + len(fun.name) + 2
			}
			for i, n := range fun.children {
				if 0 < i || !fun.loose {
					n.setNewline(true)
				}
				n.setLeft(offset)
				cw := n.reorg(edge)
				if last == i {
					cw++
				}
				if fun.wide < cw+offset-fun.x {
					fun.wide = cw + offset - fun.x
				}
			}
		}
	}
	return fun.wide
}

func (fun *Fun) adjoin(b []byte) []byte {
	b = append(b, '(')
	b = append(b, fun.name...)
	offset := fun.x + 1
	if fun.loose { // line up under first argument
		offset = fun.x + len(fun.name) + 2
	}
	for _, n := range fun.children {
		if n.newline() {
			b = append(b, indent[:offset+1]...)
		} else {
			b = append(b, ' ')
		}
		b = n.adjoin(b)
	}
	return append(b, ')')
}
