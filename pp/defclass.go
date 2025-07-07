// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

import (
	"github.com/ohler55/slip"
)

// Defclass represents a defclass block.
type Defclass struct {
	List
}

func defclassFromList(args slip.List, p *slip.Printer) Node {
	dc := Defclass{
		List: List{
			children: make([]Node, len(args)),
		},
	}
	dc.List.children[0] = buildNode(args[0], p)
	if args[1] == nil {
		dc.children[1] = &Leaf{text: []byte{'(', ')'}}
	} else if list, ok := args[1].(slip.List); ok {
		dc.List.children[1] = newList(list, p, false)
	} else {
		dc.List.children[1] = buildNode(args[1], p)
	}

	if args[2] == nil {
		dc.children[2] = &Leaf{text: []byte{'(', ')'}}
	} else if list, ok := args[1].(slip.List); ok {
		// TBD special for slot specs
		dc.List.children[2] = newList(list, p, false)
	} else {
		dc.List.children[2] = buildNode(args[2], p)
	}
	for i, v := range args[3:] {
		dc.List.children[i+3] = buildNode(v, p)
		if copt, ok := v.(slip.List); ok && 1 < len(copt) && copt[0] == slip.Symbol(":documentation") {
			if ss, ok2 := copt[1].(slip.String); ok2 {
				dc.List.children[i+3].(*List).children[1] = &Doc{text: string(ss)}
			}
		}
	}
	return &dc
}

func (dc *Defclass) layout(left int) (w int) {
	dc.x = left
	w = 10                        // (defclass + space
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
		}
		rest := dc.children[3:]
		last := len(rest) - 1
		for i, n := range rest {
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
	b = append(b, "(defclass"...)
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
