// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

import (
	"github.com/ohler55/slip"
)

// Defclass represents a defclass block.
type Defclass struct {
	List
	name string
}

func defclassFromList(name string, args slip.List, p *slip.Printer) Node {
	dc := Defclass{
		name: name,
		List: List{
			children: make([]Node, len(args)),
		},
	}
	dc.List.children[0] = buildNode(args[0], p) // class name
	// supers
	if args[1] == nil {
		dc.children[1] = &Leaf{text: []byte{'(', ')'}}
	} else if list, ok := args[1].(slip.List); ok {
		dc.List.children[1] = newList(list, p, false)
	} else {
		dc.List.children[1] = buildNode(args[1], p)
	}
	// slot specifications
	if args[2] == nil {
		dc.children[2] = &Leaf{text: []byte{'(', ')'}}
	} else if list, ok := args[2].(slip.List); ok {
		ssa := Vertical{List: List{children: make([]Node, len(list))}}
		for i, v := range list {
			switch tv := v.(type) {
			case slip.Symbol:
				ssa.List.children[i] = &Leaf{text: []byte(tv)}
			case slip.List:
				ssa.List.children[i] = newOptPairs(tv, p)
			default:
				ssa.List.children[i] = buildNode(v, p)
			}
		}
		dc.List.children[2] = &ssa
	} else {
		dc.List.children[2] = buildNode(args[2], p)
	}
	// class options
	for i, v := range args[3:] {
		if option, ok := v.(slip.List); ok && 2 <= len(option) {
			switch option[0] {
			case slip.Symbol(":documentation"):
				if ss, _ := option[1].(slip.String); 0 < len(ss) {
					dc.List.children[i+3] = &List{
						children: []Node{
							&Leaf{text: []byte(":documentation")},
							&Doc{text: string(ss)},
						},
					}
				}
			case slip.Symbol(":default-initargs"):
				dc.List.children[i+3] = newOptPairs(option, p)
			}
		}
		if dc.List.children[i+3] == nil {
			dc.List.children[i+3] = buildNode(v, p)
		}
	}
	return &dc
}

func (dc *Defclass) layout(left int) (w int) {
	dc.x = left
	w = len(dc.name) + 2          // (defclass + space
	w += dc.children[0].layout(w) // class name
	w++
	w += dc.children[1].layout(w) // supers list
	dc.children[2].setNewline(true)
	cw := dc.children[2].layout(left + 2) // slot specifications
	if w < cw+2 {
		w = cw + 2
	}
	rest := dc.children[3:]
	last := len(rest) - 1
	for i, n := range rest {
		n.setNewline(true)
		cw := n.layout(left + 2)
		if last == i {
			cw++
		}
		if w < cw {
			w = cw
		}
	}
	dc.wide = w

	return
}

func (dc *Defclass) reorg(edge int) int {
	if edge < dc.right() {
		w := dc.children[0].reorg(edge) + 10 // (defclass + space + class-name
		if edge < dc.children[1].right() {   // supers list
			dc.children[1].setNewline(true)
			dc.children[1].setLeft(dc.x + 4)
			dc.children[1].reorg(edge)
		}
		dc.children[2].setLeft(dc.x + 2)
		dc.children[2].reorg(edge)
		rest := dc.children[3:]
		last := len(rest) - 1
		for i, n := range rest {
			dc.children[i].setLeft(dc.x + 2)
			_ = n.reorg(edge)
			r := n.right()
			if i == last {
				r++
			}
			if w < r-dc.x {
				w = r - dc.x
			}
		}
		dc.wide = w
	}
	return dc.wide
}

func (dc *Defclass) adjoin(b []byte) []byte {
	b = append(b, '(')
	b = append(b, dc.name...)
	for _, n := range dc.children {
		if n.newline() {
			b = append(b, indent[:n.left()+1]...)
		} else {
			b = append(b, ' ')
		}
		b = n.adjoin(b)
	}
	return append(b, ')')
}
