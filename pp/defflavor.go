// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

import (
	"github.com/ohler55/slip"
)

// Defflavor represents a defflavor block.
type Defflavor struct {
	List
}

func defflavorFromList(args slip.List, p *slip.Printer) Node {
	df := Defflavor{
		List: List{
			children: make([]Node, len(args)),
		},
	}
	for i, v := range args {
		if i == 1 || i == 2 { // instance variables or inherited flavors
			if list, ok := v.(slip.List); ok && len(list) == 0 {
				df.children[i] = &Leaf{text: []byte{'(', ')'}}
				continue
			}
		}
		df.children[i] = buildNode(v, p)
	}
	for len(df.children) < 3 {
		df.children = append(df.children, &Leaf{text: []byte{'(', ')'}})
	}
	return &df
}

func (df *Defflavor) layout(left int) (w int) {
	df.x = left
	w = 11                        // (defflavor + space
	w += df.children[0].layout(w) // flavor name
	w++
	ax := w
	w += df.children[1].layout(w) // variables list
	df.children[2].setNewline(true)
	cw := df.children[2].layout(ax) // inherited flavor list
	if w < cw+ax {
		w = cw + ax
	}
	rest := df.children[3:]
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
	df.wide = w

	return
}

func (df *Defflavor) reorg(edge int) int {
	if edge < df.right() {
		w := df.children[0].reorg(edge) + 11 // (defflavor + space + flavor-name
		if edge < df.children[1].right() {   // instance variable list
			df.children[1].setNewline(true)
			df.children[1].setLeft(df.x + 4)
			df.children[2].setLeft(df.x + 4) // inherited flavors
		}
		last := len(df.children) - 1
		for i, n := range df.children {
			_ = n.reorg(edge)
			r := n.right()
			if i == last {
				r++
			}
			if w < r-df.x {
				w = r - df.x
			}
		}
		df.wide = w
	}
	return df.wide
}

func (df *Defflavor) adjoin(b []byte) []byte {
	b = append(b, "(defflavor"...)
	for _, n := range df.children {
		if n.newline() {
			b = append(b, indent[:n.left()+1]...)
		} else {
			b = append(b, ' ')
		}
		b = n.adjoin(b)
	}
	return append(b, ')')
}
