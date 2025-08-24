// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

import (
	"github.com/ohler55/slip"
)

// MethodOption represents a generic :method option block.
type MethodOption struct {
	List // forms
	qual *Leaf
	sll  Node
	doc  *Doc
}

func methodOption(args slip.List, p *slip.Printer) Node {
	var mo MethodOption
	// expect name [qualifier] specifiers [doc] forms*
	if sym, _ := args[0].(slip.Symbol); 0 < len(sym) {
		mo.qual = &Leaf{text: []byte(sym)}
		args = args[1:]
	}
	mo.sll = argsFromList(args[0], p)
	args = args[1:]
	if ss, ok := args[0].(slip.String); ok {
		mo.doc = &Doc{text: string(ss), nl: true}
		args = args[1:]
	}
	for _, v := range args {
		mo.children = append(mo.children, buildNode(v, p))
	}
	return &mo
}

func (mo *MethodOption) layout(left int) (w int) {
	mo.x = left
	w = 9
	if mo.qual != nil {
		w += mo.qual.layout(w)
		w++
	}
	w += mo.sll.layout(w)
	if mo.doc != nil {
		_ = mo.doc.layout(left + 2)
	}
	last := len(mo.children) - 1
	for i, n := range mo.children {
		n.setNewline(true)
		cw := n.layout(left + 2)
		if last == i {
			cw++
		}
		if w < cw {
			w = cw
		}
	}
	mo.wide = w

	return
}

func (mo *MethodOption) reorg(edge int) int {
	if edge < mo.right() {
		w := 9
		if mo.qual != nil && edge < mo.qual.right() {
			mo.qual.setNewline(true)
			mo.qual.setLeft(mo.x + 4)
			mo.sll.setNewline(true)
			mo.sll.setLeft(mo.x + 4)
		} else if edge < mo.sll.right() {
			mo.sll.setNewline(true)
			mo.sll.setLeft(mo.x + 4)
		}
		_ = mo.sll.reorg(edge)
		r := mo.sll.right()
		if w < r-mo.x {
			w = r - mo.x
		}
		if mo.doc != nil {
			w = mo.doc.reorg(edge)
		}
		last := len(mo.children) - 1
		for i, n := range mo.children {
			_ = n.reorg(edge)
			r = n.right()
			if i == last {
				r++
			}
			if w < r-mo.x {
				w = r - mo.x
			}
		}
		mo.wide = w
	}
	return mo.wide
}

func (mo *MethodOption) adjoin(b []byte) []byte {
	b = append(b, "(:method"...)
	if mo.qual != nil {
		if mo.qual.newline() {
			b = append(b, indent[:mo.qual.left()+1]...)
		} else {
			b = append(b, ' ')
		}
		b = mo.qual.adjoin(b)
	}
	if mo.sll.newline() {
		b = append(b, indent[:mo.sll.left()+1]...)
	} else {
		b = append(b, ' ')
	}
	b = mo.sll.adjoin(b)
	if mo.doc != nil {
		b = append(b, indent[:mo.doc.left()+1]...)
		b = mo.doc.adjoin(b)
	}
	for _, n := range mo.children {
		b = append(b, indent[:n.left()+1]...)
		b = n.adjoin(b)
	}
	return append(b, ')')
}
